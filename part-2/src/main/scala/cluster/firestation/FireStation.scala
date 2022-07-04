package cluster.firestation

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
  case class NotifyAlarmOn() extends Event with CborSerializable
  case class NotifyAlarmOff() extends Event with CborSerializable
  val FireStationServiceKey: ServiceKey[FireStation.Event] = ServiceKey[FireStation.Event]("FireStation")

  def apply(): Behavior[FireStation.Event] =
    Behaviors setup { ctx =>
      val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
        case RainGauge.ListenerServiceKey.Listing(newSet) =>
          RainGaugesUpdated(newSet)
      }
      ctx.system.receptionist ! Receptionist.Subscribe(RainGauge.ListenerServiceKey, subscriptionAdapter)
      ctx.system.receptionist ! Receptionist.Register(FireStationServiceKey, ctx.self)
      running(ctx, IndexedSeq.empty, 0)
    }

  private def running(ctx: ActorContext[FireStation.Event],
                      rainGauges: IndexedSeq[ActorRef[RainGauge.Event]],
                      alarmNotifications: Int): Behavior[FireStation.Event] =
    Behaviors receiveMessage { msg => msg match
      case RainGaugesUpdated(newSet) =>
        ctx.log.info(s"There are now ${rainGauges.size} rain gauges")
        running(ctx,newSet.toIndexedSeq, alarmNotifications)
      case NotifyAlarmOn() =>
        val notifications = alarmNotifications + 1
        ctx.log.info(s"Received $notifications notifications")
        if notifications >= rainGauges.size then
          ctx.log.info("Taking care of the alarm")
          running(ctx, rainGauges, 0)
        else
          running(ctx, rainGauges, notifications)
      case NotifyAlarmOff() => Behaviors.same
    }