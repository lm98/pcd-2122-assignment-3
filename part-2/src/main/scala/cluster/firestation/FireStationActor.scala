package cluster.firestation

import cluster.view.ViewActor
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import cluster.CborSerializable
import cluster.raingauge.RainGaugeActor

import concurrent.duration.*

object FireStationActor:
  sealed trait Event
  private case class ViewActorsUpdated(newSet: Set[ActorRef[ViewActor.Event]]) extends Event
  private case class Tick() extends Event
  case class ZoneRequestRainGaugeToFireStation(zone: Int, replyTo: ActorRef[RainGaugeActor.Event]) extends Event with CborSerializable
  case class NotifyAlarmOn() extends Event with CborSerializable
  case class ManageAlarm(zoneID: Int) extends Event with CborSerializable
  val FireStationServiceKey: ServiceKey[FireStationActor.Event] = ServiceKey[FireStationActor.Event]("FireStation")

  def apply(zone: Int = 1): Behavior[FireStationActor.Event] =
    Behaviors withTimers { timers =>
      Behaviors setup { ctx =>
        val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
          case ViewActor.ViewActorServiceKey.Listing(newSet) =>
            ViewActorsUpdated(newSet)
        }
        ctx.system.receptionist ! Receptionist.Subscribe(ViewActor.ViewActorServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Register(FireStationServiceKey, ctx.self)

        timers.startTimerWithFixedDelay(Tick(), Tick(), 10.seconds)

        running(ctx, Set.empty, IndexedSeq.empty, 0, zone)
      }
    }

  private def running(ctx: ActorContext[Event],
                      rainGauges: Set[ActorRef[RainGaugeActor.Event]],
                      viewActors: IndexedSeq[ActorRef[ViewActor.Event]],
                      alarmNotifications: Int,
                      zone: Int): Behavior[FireStationActor.Event] =
    Behaviors receiveMessage { msg => msg match
      case ViewActorsUpdated(newSet) =>
        ctx.log.info(s"Views have been updated to ${newSet.size}")
        running(ctx, rainGauges, newSet.toIndexedSeq, alarmNotifications, zone)
      case ZoneRequestRainGaugeToFireStation(originZone, rainGauge) =>
        if originZone == zone then
          rainGauge ! RainGaugeActor.ZoneRequestFireStationToRainGauge(ctx.self)
          running(ctx, rainGauges + rainGauge, viewActors, alarmNotifications, zone)
        else
          Behaviors.same
      case NotifyAlarmOn() =>
        val notifications = alarmNotifications + 1
        ctx.log.info(s"Firestation #$zone Received $notifications notifications")
        if notifications >= rainGauges.size then
          ctx.log.info(s"Firestation #$zone Warned")
          viewActors foreach { _ ! ViewActor.AlarmOn(zone) }
          warned(ctx, rainGauges, viewActors, zone)
        else
          running(ctx, rainGauges, viewActors, notifications, zone)
      case _ => Behaviors.same
    }

  private def warned(ctx: ActorContext[Event],
                      rainGauges: Set[ActorRef[RainGaugeActor.Event]],
                      viewActors: IndexedSeq[ActorRef[ViewActor.Event]],
                      zone: Int): Behavior[FireStationActor.Event] =
    Behaviors receiveMessage { msg => msg match
      case ManageAlarm(zoneID) =>
        if(zoneID == zone)
          ctx.log.info(s"Firestation #$zone Going to manage the alarm")
          viewActors foreach { _ ! ViewActor.FireStationBusy(zone)}
          busy(ctx, rainGauges, viewActors, zone)
        else
          Behaviors.same
      case _ => Behaviors.same
    }
    
  private def busy(ctx: ActorContext[Event],
                   rainGauges: Set[ActorRef[RainGaugeActor.Event]],
                   viewActors: IndexedSeq[ActorRef[ViewActor.Event]],
                   zone: Int): Behavior[FireStationActor.Event] =
    Behaviors receiveMessage { msg => msg match
      case Tick() =>
        ctx.log.info(s"Firestation #$zone Alarm managed")
        viewActors foreach { _ ! ViewActor.AlarmOff(zone) }
        viewActors foreach { _ ! ViewActor.FireStationFree(zone) }
        running(ctx, rainGauges, viewActors, 0, zone)
      case _ =>
        ctx.log.warn(s"Firestation #$zone is currently busy")
        Behaviors.same
    }