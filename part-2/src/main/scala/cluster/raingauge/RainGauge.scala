package cluster.raingauge

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import cluster.CborSerializable
import cluster.firestation.FireStation
import cluster.firestation.FireStation.{FireStationServiceKey, NotifyAlarmOn}

import concurrent.duration.*
import scala.util.Random

object RainGauge:
  sealed trait Event
  private case class ListenersUpdated(newSet: Set[ActorRef[Event]]) extends Event
  private case class FireStationsUpdated(newSet: Set[ActorRef[NotifyAlarmOn]]) extends Event
  private case class Tick() extends Event
  case class ReceiveValue(value: Double) extends Event with CborSerializable
  case class NotifyAlarmOff() extends Event with CborSerializable

  val ListenerServiceKey: ServiceKey[Event] = ServiceKey[Event]("Listener")

  def apply(): Behavior[Event] =
    Behaviors setup { ctx =>
      Behaviors withTimers { timers =>

        val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
          case RainGauge.ListenerServiceKey.Listing(newListeners) =>
            ListenersUpdated(newListeners.filterNot(_.equals(ctx.self)))
          case FireStation.FireStationServiceKey.Listing(newFireStations) =>
            FireStationsUpdated(newFireStations)
        }
        ctx.system.receptionist ! Receptionist.Subscribe(ListenerServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Subscribe(FireStationServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Register(ListenerServiceKey, ctx.self)

        timers.startTimerWithFixedDelay(Tick(), Tick(), 2.seconds)

        running(ctx, IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty)
      }
    }

  private def running(ctx: ActorContext[RainGauge.Event],
                      listeners: IndexedSeq[ActorRef[Event]],
                      fireStations:IndexedSeq[ActorRef[NotifyAlarmOn]],
                      values: IndexedSeq[Double]): Behavior[Event] =

    def checkAlarmCondition(values: IndexedSeq[Double]): Boolean =
      values.count(_ > 0.5) >= (ctx.system.settings.config.getInt("rain-analysis.rainGaugesPerNode")/2 + 1)

    Behaviors receiveMessage  { msg => msg match
      case ListenersUpdated(gauges) =>
        running(ctx, gauges.toIndexedSeq, fireStations,values)
      case FireStationsUpdated(newSet) =>
        ctx.log.info(s"there are now ${fireStations.size} fire stations")
        running(ctx, listeners, newSet.toIndexedSeq, values)
      case Tick() =>
        val value = Random.nextDouble()
        val newValues = values :+ value
        listeners foreach { _ ! ReceiveValue(value) }
        checkAlarmCondition(newValues) match
          case false if newValues.size >= listeners.size => running(ctx, listeners, fireStations, IndexedSeq.empty)
          case false if newValues.size < listeners.size => running(ctx, listeners, fireStations, newValues)
          case true => alarmed(listeners, fireStations)
      case ReceiveValue(value) =>
        ctx.log.info(s"Received $value")
        running(ctx, listeners, fireStations, values :+ value)
    }

  private def alarmed(listeners: IndexedSeq[ActorRef[Event]],
                      fireStations: IndexedSeq[ActorRef[NotifyAlarmOn]]): Behavior[Event] =
    Behaviors setup { ctx =>

      fireStations foreach { _ ! NotifyAlarmOn() }

      Behaviors receiveMessage { msg => msg match
        case NotifyAlarmOff() => ctx.log.info("Alarm managed, returning to normal behavior") ; running(ctx, listeners, fireStations, IndexedSeq.empty)
        case FireStationsUpdated(newSet) => alarmed(listeners, newSet.toIndexedSeq)
        case _ => ctx.log.warn(s"I'm alarmed") ; Behaviors.same
      }
    }