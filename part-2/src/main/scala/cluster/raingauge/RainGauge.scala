package cluster.raingauge

import RainGauge.running
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import cluster.CborSerializable

import concurrent.duration.*
import scala.util.Random

object RainGauge:
  sealed trait Event
  private case class ListenersUpdated(newSet: Set[ActorRef[ReceiveValue]]) extends Event
  private case class Tick() extends Event
  case class ReceiveValue(value: Double) extends Event with CborSerializable

  val ListenerServiceKey: ServiceKey[ReceiveValue] = ServiceKey[ReceiveValue]("Listener")

  def apply(): Behavior[Event] =
    Behaviors setup { ctx =>
      Behaviors withTimers { timers =>

        val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
          case RainGauge.ListenerServiceKey.Listing(newListeners) => ListenersUpdated(newListeners.filterNot(_.equals(ctx.self)))
        }
        ctx.system.receptionist ! Receptionist.Subscribe(ListenerServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Register(ListenerServiceKey, ctx.self)

        timers.startTimerWithFixedDelay(Tick(), Tick(), 2.seconds)

        running(ctx, IndexedSeq.empty)
      }
    }

  def running(ctx: ActorContext[RainGauge.Event], listeners: IndexedSeq[ActorRef[ReceiveValue]]): Behavior[Event] =
    Behaviors receiveMessage  { msg => msg match
      case ListenersUpdated(gauges) =>
        ctx.log.info(s"There are now ${gauges.size} other rain gauges registered in the cluster")
        running(ctx, gauges.toIndexedSeq)
      case Tick() =>
        listeners foreach { _ ! ReceiveValue(Random.nextDouble()) }
        Behaviors.same
      case ReceiveValue(value) =>
        ctx.log.info(s"Received $value")
        Behaviors.same
    }
