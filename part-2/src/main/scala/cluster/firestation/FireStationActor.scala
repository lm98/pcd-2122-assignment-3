package cluster.firestation

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import cluster.CborSerializable
import cluster.raingauge.RainGaugeActor
import cluster.view.ViewActor

import concurrent.duration.*

object FireStationActor:
  sealed trait Event
  private case class ViewActorsUpdated(newSet: Set[ActorRef[ViewActor.Event]]) extends Event
  case class ZoneRequestRainGaugeToFireStation(zone: Int, replyTo: ActorRef[RainGaugeActor.Event]) extends Event with CborSerializable
  case class NotifyAlarmOn() extends Event with CborSerializable
  case class NotifyAlarmOff() extends Event with CborSerializable
  val FireStationServiceKey: ServiceKey[FireStationActor.Event] = ServiceKey[FireStationActor.Event]("FireStation")

  def apply(zone: Int = 1): Behavior[FireStationActor.Event] =
    Behaviors setup { ctx =>
      val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
        case ViewActor.ViewActorServiceKey.Listing(newSet) =>
          ViewActorsUpdated(newSet)
      }
      ctx.system.receptionist ! Receptionist.Subscribe(ViewActor.ViewActorServiceKey, subscriptionAdapter)
      ctx.system.receptionist ! Receptionist.Register(FireStationServiceKey, ctx.self)

      running(ctx, Set.empty, IndexedSeq.empty, 0, zone)
    }

  private def running(ctx: ActorContext[Event],
                      rainGauges: Set[ActorRef[RainGaugeActor.Event]],
                      viewActors: IndexedSeq[ActorRef[ViewActor.Event]],
                      alarmNotifications: Int,
                      zone: Int): Behavior[FireStationActor.Event] =
    Behaviors receiveMessage { msg => msg match
      case ViewActorsUpdated(newSet) =>
        running(ctx, rainGauges, newSet.toIndexedSeq, alarmNotifications, zone)
      case ZoneRequestRainGaugeToFireStation(originZone, rainGauge) =>
        if originZone == zone then
          rainGauge ! RainGaugeActor.ZoneRequestFireStationToRainGauge(ctx.self)
          running(ctx, rainGauges + rainGauge, viewActors, alarmNotifications, zone)
        else
          Behaviors.same
      case NotifyAlarmOn() =>
        val notifications = alarmNotifications + 1
        ctx.log.info(s"Received $notifications notifications")
        if notifications >= rainGauges.size then
          ctx.log.info("Taking care of the alarm")
          viewActors foreach { _ ! ViewActor.AlarmOn(zone) }
          alarmed(ctx, rainGauges, viewActors, zone)
        else
          running(ctx, rainGauges, viewActors, notifications, zone)
      case _ => Behaviors.same
    }

  private def alarmed(ctx: ActorContext[Event],
                      rainGauges: Set[ActorRef[RainGaugeActor.Event]],
                      viewActors: IndexedSeq[ActorRef[ViewActor.Event]],
                      zone: Int): Behavior[FireStationActor.Event] =
    Behaviors receiveMessage { msg => msg match
      case NotifyAlarmOff() =>
        ctx.log.info("Alarm managed")
        viewActors foreach { _ ! ViewActor.AlarmOff(zone) }
        running(ctx, rainGauges, viewActors, 0, zone)
      case _ => Behaviors.same
    }