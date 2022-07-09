package cluster.firestation

import actors.ViewActor
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
  case class ManageAlarm() extends Event with CborSerializable
  val FireStationServiceKey: ServiceKey[FireStationActor.Event] = ServiceKey[FireStationActor.Event]("FireStation")

  def apply(zone: Int = 1): Behavior[FireStationActor.Event] =
    Behaviors withTimers { timers =>
      Behaviors setup { ctx =>
        val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
          case ViewActor.viewActorServiceKey.Listing(newSet) =>
            ViewActorsUpdated(newSet)
        }
        ctx.system.receptionist ! Receptionist.Subscribe(ViewActor.viewActorServiceKey, subscriptionAdapter)
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
          ctx.log.info("FireStation Warned")
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
      case ManageAlarm() =>
        ctx.log.info("Going to manage the alarm")
        busy(ctx, rainGauges, viewActors, zone)
      case _ => Behaviors.same
    }
    
  private def busy(ctx: ActorContext[Event],
                   rainGauges: Set[ActorRef[RainGaugeActor.Event]],
                   viewActors: IndexedSeq[ActorRef[ViewActor.Event]],
                   zone: Int): Behavior[FireStationActor.Event] =
    Behaviors receiveMessage { msg => msg match
      case Tick() =>
        ctx.log.info("Alarm managed")
        viewActors foreach { _ ! ViewActor.AlarmOff(zone) }
        running(ctx, rainGauges, viewActors, 0, zone)
      case _ =>
        ctx.log.warn("FireStation is currently busy")
        Behaviors.same
    }