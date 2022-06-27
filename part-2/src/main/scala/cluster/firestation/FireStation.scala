package cluster.firestation

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import cluster.CborSerializable
import cluster.raingauge.RainGauge
import cluster.raingauge.RainGauge.{ListenerServiceKey, NotifyAlarmOff, ReceiveValue}

import concurrent.duration.*

object FireStation:
  sealed trait Event
  private case class RainGaugesUpdated(newSet: Set[ActorRef[RainGauge.Event]]) extends Event
  private case class Tick() extends Event
  case class NotifyAlarmOn() extends Event with CborSerializable

  val FireStationServiceKey: ServiceKey[NotifyAlarmOn] = ServiceKey[NotifyAlarmOn]("FireStation")

  def apply(): Behavior[Event] =
    Behaviors setup { ctx =>
      Behaviors withTimers { timers =>
        val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
          case RainGauge.ListenerServiceKey.Listing(newSet) =>
            RainGaugesUpdated(newSet)
        }
        ctx.system.receptionist ! Receptionist.Subscribe(RainGauge.ListenerServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Register(FireStationServiceKey, ctx.self)

        timers.startTimerWithFixedDelay(Tick(), Tick(), 5.seconds)

        running(ctx, IndexedSeq.empty, 0)
      }
    }

  private def running(ctx: ActorContext[Event], rainGauges: IndexedSeq[ActorRef[RainGauge.Event]], alarmNotifications: Int): Behavior[Event] =
    Behaviors receiveMessage { msg => msg match
      case RainGaugesUpdated(newSet) =>
        ctx.log.info(s"There are now ${rainGauges.size} rain gauges")
        running(ctx, newSet.toIndexedSeq, alarmNotifications)
      case NotifyAlarmOn() =>
        val notifications = alarmNotifications + 1
        ctx.log.info(s"Received $notifications notifications")
        if notifications >= ctx.system.settings.config.getInt("rain-analysis.rainGaugesPerNode") then
          alarmManagement(ctx, rainGauges)
        else
          running(ctx, rainGauges, notifications)

      case _ => Behaviors.same
    }

  private def alarmManagement(ctx: ActorContext[Event], rainGauges: IndexedSeq[ActorRef[RainGauge.Event]]): Behavior[Event] =
    Behaviors receiveMessage { msg => msg match
      case Tick() =>
        ctx.log.info("Taking care of the alarm")
        rainGauges foreach { _ ! NotifyAlarmOff() }
        running(ctx, rainGauges, 0)
    }