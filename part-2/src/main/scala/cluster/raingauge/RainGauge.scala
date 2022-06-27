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

        running(ctx, IndexedSeq.empty, IndexedSeq.empty)
      }
    }

  def running(ctx: ActorContext[RainGauge.Event], listeners: IndexedSeq[ActorRef[ReceiveValue]], values: IndexedSeq[Double]): Behavior[Event] =
    def checkAlarmCondition(values: IndexedSeq[Double]): Boolean =
      values.count(_ > 0.5) >= (ctx.system.settings.config.getInt("rain-analysis.rainGaugesPerNode")/2 + 1)

    Behaviors receiveMessage  { msg => msg match
      case ListenersUpdated(gauges) =>
        running(ctx, gauges.toIndexedSeq, values)
      case Tick() =>
        val value = Random.nextDouble()
        val newValues = values :+ value
        listeners foreach { _ ! ReceiveValue(value) }
        checkAlarmCondition(newValues) match
          case false if newValues.size >= listeners.size => running(ctx, listeners, IndexedSeq.empty)
          case false if newValues.size < listeners.size => running(ctx, listeners, newValues)
          case true => alarmed(ctx)
      case ReceiveValue(value) =>
        ctx.log.info(s"Received $value")
        running(ctx, listeners, values :+ value)
    }

  def alarmed(ctx: ActorContext[RainGauge.Event]): Behavior[Event] =
    Behaviors receiveMessage { msg => msg match
      case _ => ctx.log.warn("I'm alarmed") ; Behaviors.same
    }