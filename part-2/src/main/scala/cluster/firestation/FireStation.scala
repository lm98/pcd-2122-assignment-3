package cluster.firestation

import actors.ViewActor
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import cluster.CborSerializable
import cluster.raingauge.RainGauge
import cluster.raingauge.RainGauge.ListenerServiceKey

import concurrent.duration.*

object FireStation:
  sealed trait Event
  private case class RainGaugesUpdated(newSet: Set[ActorRef[RainGauge.Event]]) extends Event
  private case class ViewActorsUpdated(newSet: Set[ActorRef[ViewActor.Event]]) extends Event
  case class NotifyAlarmOn() extends Event with CborSerializable
  case class NotifyAlarmOff() extends Event with CborSerializable
  val FireStationServiceKey: ServiceKey[FireStation.Event] = ServiceKey[FireStation.Event]("FireStation")

  def apply(): Behavior[FireStation.Event] =
    Behaviors setup { ctx =>
      val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
        case RainGauge.ListenerServiceKey.Listing(newSet) =>
          RainGaugesUpdated(newSet)
        case ViewActor.viewActorServiceKey.Listing(newSet) =>
          ViewActorsUpdated(newSet)
      }
      ctx.system.receptionist ! Receptionist.Subscribe(RainGauge.ListenerServiceKey, subscriptionAdapter)
      ctx.system.receptionist ! Receptionist.Subscribe(ViewActor.viewActorServiceKey, subscriptionAdapter)
      ctx.system.receptionist ! Receptionist.Register(FireStationServiceKey, ctx.self)

      running(ctx, IndexedSeq.empty, IndexedSeq.empty,0)
    }

  private def running(ctx: ActorContext[FireStation.Event],
                      rainGauges: IndexedSeq[ActorRef[RainGauge.Event]],
                      viewActors: IndexedSeq[ActorRef[ViewActor.Event]],
                      alarmNotifications: Int): Behavior[FireStation.Event] =
    Behaviors receiveMessage { msg => msg match
      case RainGaugesUpdated(newSet) =>
        ctx.log.info(s"There are now ${rainGauges.size} rain gauges")
        running(ctx,newSet.toIndexedSeq, viewActors, alarmNotifications)
      case ViewActorsUpdated(newSet) =>
        running(ctx, rainGauges, newSet.toIndexedSeq, alarmNotifications)
      case NotifyAlarmOn() =>
        val notifications = alarmNotifications + 1
        ctx.log.info(s"Received $notifications notifications")
        if notifications >= rainGauges.size then
          ctx.log.info("Taking care of the alarm")
          viewActors foreach { _ ! ViewActor.AlarmOn() }
          alarmed(ctx, rainGauges, viewActors)
        else
          running(ctx, rainGauges, viewActors, notifications)
      case _ => Behaviors.same
    }

  private def alarmed(ctx: ActorContext[FireStation.Event],
                      rainGauges: IndexedSeq[ActorRef[RainGauge.Event]],
                      viewActors: IndexedSeq[ActorRef[ViewActor.Event]]): Behavior[FireStation.Event] =
    Behaviors receiveMessage { msg => msg match
      case NotifyAlarmOff() =>
        ctx.log.info("Alarm managed")
        running(ctx, rainGauges, viewActors, 0)
      case _ => Behaviors.same
    }