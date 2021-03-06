package cluster.firestation

import cluster.view.ViewActor
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import cluster.CborSerializable
import cluster.raingauge.RainGaugeActor
import model.{FireStation, FireStationState}

import concurrent.duration.*

object FireStationActor:
  sealed trait Event
  private case class ViewActorsUpdated(newSet: Set[ActorRef[ViewActor.Event]]) extends Event
  private case class Tick() extends Event
  case class ZoneRequestRainGaugeToFireStation(zone: Int, replyTo: ActorRef[RainGaugeActor.Event]) extends Event with CborSerializable
  case class NotifyAlarmOn() extends Event with CborSerializable
  case class ManageAlarm(zoneID: Int) extends Event with CborSerializable
  val FireStationServiceKey: ServiceKey[FireStationActor.Event] = ServiceKey[FireStationActor.Event]("FireStation")

  def apply(fireStation: FireStation): Behavior[FireStationActor.Event] =
    Behaviors withTimers { timers =>
      Behaviors setup { ctx =>
        val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
          case ViewActor.ViewActorServiceKey.Listing(newSet) =>
            ViewActorsUpdated(newSet)
        }
        ctx.system.receptionist ! Receptionist.Subscribe(ViewActor.ViewActorServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Register(FireStationServiceKey, ctx.self)

        timers.startTimerWithFixedDelay(Tick(), Tick(), 10.seconds)

        running(ctx, Set.empty, Set.empty, 0, fireStation)
      }
    }

  private def running(ctx: ActorContext[Event],
                      rainGauges: Set[ActorRef[RainGaugeActor.Event]],
                      viewActors: Set[ActorRef[ViewActor.Event]],
                      alarmNotifications: Int,
                      fireStation: FireStation): Behavior[FireStationActor.Event] =
    Behaviors receiveMessage { msg => msg match
      case ViewActorsUpdated(newSet) =>
        newSet foreach { _ ! ViewActor.AddFireStation(fireStation) }
        ctx.log.info(s"Views have been updated to ${newSet.size}")
        running(ctx, rainGauges, newSet, alarmNotifications, fireStation)
      case ZoneRequestRainGaugeToFireStation(originZone, rainGauge) =>
        if originZone == fireStation.zoneID then
          rainGauge ! RainGaugeActor.ZoneRequestFireStationToRainGauge(ctx.self)
          running(ctx, rainGauges + rainGauge, viewActors, alarmNotifications, fireStation)
        else
          Behaviors.same
      case NotifyAlarmOn() =>
        val notifications = alarmNotifications + 1
        ctx.log.info(s"Firestation #${fireStation.zoneID} Received $notifications notifications")
        if notifications >= (rainGauges.size/2 + 1) then
          ctx.log.info(s"Firestation #${fireStation.zoneID} Warned")
          viewActors foreach { _ ! ViewActor.AlarmOn(fireStation.zoneID) }
          fireStation.changeState(FireStationState.Warned)
          viewActors foreach { _ ! ViewActor.UpdateStation(fireStation) }
          warned(ctx, rainGauges, viewActors, fireStation)
        else
          running(ctx, rainGauges, viewActors, notifications, fireStation)
      case _ => Behaviors.same
    }

  private def warned(ctx: ActorContext[Event],
                      rainGauges: Set[ActorRef[RainGaugeActor.Event]],
                      viewActors: Set[ActorRef[ViewActor.Event]],
                     fireStation: FireStation): Behavior[FireStationActor.Event] =
    Behaviors receiveMessage { msg => msg match
      case ViewActorsUpdated(newSet) =>
        newSet foreach { _ ! ViewActor.AddFireStation(fireStation) }
        ctx.log.info(s"Views have been updated to ${newSet.size}")
        warned(ctx, rainGauges, newSet, fireStation)
      case ManageAlarm(zoneID) =>
        if(zoneID == fireStation.zoneID)
          ctx.log.info(s"Firestation #${fireStation.zoneID} Going to manage the alarm")
          fireStation.changeState(FireStationState.Busy)
          viewActors foreach { _ ! ViewActor.UpdateStation(fireStation)}
          busy(ctx, rainGauges, viewActors, fireStation)
        else
          Behaviors.same
      case _ => Behaviors.same
    }
    
  private def busy(ctx: ActorContext[Event],
                   rainGauges: Set[ActorRef[RainGaugeActor.Event]],
                   viewActors: Set[ActorRef[ViewActor.Event]],
                   fireStation: FireStation): Behavior[FireStationActor.Event] =
    Behaviors receiveMessage { msg => msg match
      case ViewActorsUpdated(newSet) =>
        newSet foreach { _ ! ViewActor.AddFireStation(fireStation) }
        ctx.log.info(s"Views have been updated to ${newSet.size}")
        busy(ctx, rainGauges, newSet, fireStation)
      case Tick() =>
        ctx.log.info(s"Firestation #${fireStation.zoneID} Alarm managed")
        fireStation.changeState(FireStationState.Free)
        viewActors foreach { vA =>
          vA ! ViewActor.AlarmOff(fireStation.zoneID)
          vA ! ViewActor.UpdateStation(fireStation)
        }
        running(ctx, rainGauges, viewActors, 0, fireStation)
      case _ =>
        ctx.log.warn(s"Firestation #${fireStation.zoneID} is currently busy")
        Behaviors.same
    }