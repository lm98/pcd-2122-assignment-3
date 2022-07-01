package cluster.raingauge

import akka.actor.typed.{ActorRef, Behavior, Scheduler}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.util.Timeout
import cluster.CborSerializable
import cluster.firestation.FireStation
import cluster.firestation.FireStation.{FireStationServiceKey, NotifyAlarmOn}

import java.util.concurrent.ThreadLocalRandom
import concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success}

enum ResponseValue:
  case Ok(value: Double)
  case Unreachable

import ResponseValue.*

object RainGauge:
  sealed trait Event
  private case class RainGaugesUpdated(newSet: Set[ActorRef[Event]]) extends Event
  private case class FireStationsUpdated(newSet: Set[ActorRef[NotifyAlarmOn]]) extends Event
  private case class Tick() extends Event
  private case class Request(replyTo: ActorRef[Response]) extends Event with CborSerializable
  private case class Response(value: Double) extends Event with CborSerializable
  private case class ReceiveValue(responseValue: ResponseValue) extends Event with CborSerializable
  case class NotifyAlarmOff() extends Event with CborSerializable

  val ListenerServiceKey: ServiceKey[Event] = ServiceKey[Event]("Listener")
  val ALARM_THRESHOLD = 0.60

  def apply(): Behavior[Event] =
    Behaviors setup { ctx =>
      Behaviors withTimers { timers =>

        val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
          case RainGauge.ListenerServiceKey.Listing(newGauges) =>
            RainGaugesUpdated(newGauges)
          case FireStation.FireStationServiceKey.Listing(newFireStations) =>
            FireStationsUpdated(newFireStations)
        }
        ctx.system.receptionist ! Receptionist.Subscribe(ListenerServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Subscribe(FireStationServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Register(ListenerServiceKey, ctx.self)

        timers.startTimerWithFixedDelay(Tick(), Tick(), 2.seconds)

        running(ctx, IndexedSeq.empty, IndexedSeq.empty, 0.0, IndexedSeq.empty)
      }
    }

  private def running(ctx: ActorContext[RainGauge.Event],
                      rainGauges: IndexedSeq[ActorRef[Event]],
                      fireStations:IndexedSeq[ActorRef[NotifyAlarmOn]],
                      lastValue: Double,
                      tmpValues: IndexedSeq[ResponseValue]): Behavior[Event] =

    def checkAlarmCondition(values: IndexedSeq[ResponseValue]): Boolean =
      values.count { case Ok(value) if value > ALARM_THRESHOLD => true ; case _ => false } >= (rainGauges.size/2 + 1)

    Behaviors receiveMessage  { msg => msg match
      case RainGaugesUpdated(gauges) =>
        running(ctx, gauges.toIndexedSeq, fireStations, lastValue, tmpValues)
      case FireStationsUpdated(newSet) =>
        running(ctx, rainGauges, newSet.toIndexedSeq, lastValue, tmpValues)
      case Tick() =>
        //Check other rain gauges values
        given Timeout = 2.seconds
        rainGauges.foreach(l => ctx.ask(l, Request.apply){
          case Success(Response(value)) => ReceiveValue(Ok(value))
          case _ => ReceiveValue(Unreachable)
        })
        //Update the value
        val value = ThreadLocalRandom.current().nextDouble()
        ctx.log.info(s"${ctx.self.path.name}: Produced $value")
        running(ctx, rainGauges, fireStations, value, tmpValues)
      case Request(replyTo) => replyTo ! Response(lastValue) ; Behaviors.same
      case ReceiveValue(value) =>
        val newValues = tmpValues :+ value
        checkAlarmCondition(newValues) match
          case true => alarmed(rainGauges, fireStations, lastValue)
          case false if newValues.size < rainGauges.size => running(ctx, rainGauges, fireStations, lastValue, newValues)
          case _ => running(ctx, rainGauges, fireStations, lastValue, IndexedSeq.empty)
      case _ => Behaviors.same
    }

  private def alarmed(rainGauges: IndexedSeq[ActorRef[Event]],
                      fireStations:IndexedSeq[ActorRef[NotifyAlarmOn]],
                      lastValue: Double): Behavior[Event] =
    Behaviors setup { ctx =>

      fireStations foreach { _ ! NotifyAlarmOn() }

      Behaviors receiveMessage { msg => msg match
        case RainGaugesUpdated(newSet) => running(ctx, newSet.toIndexedSeq, fireStations, lastValue, IndexedSeq.empty)
        case FireStationsUpdated(newSet) => alarmed(rainGauges, newSet.toIndexedSeq, lastValue)
        case Tick() =>
          val value = ThreadLocalRandom.current().nextDouble()
          ctx.log.warn(s"${ctx.self.path.name}: Still alarmed. Produced $value")
          alarmed(rainGauges, fireStations, value)
        case Request(replyTo) => replyTo ! Response(lastValue) ; Behaviors.same
        case NotifyAlarmOff() =>
          ctx.log.info(s"${ctx.self.path.name}: Alarm managed, returning to normal behavior")
          running(ctx, rainGauges, fireStations, lastValue, IndexedSeq.empty)
        case _ => ctx.log.warn(s"${ctx.self.path.name}: I'm alarmed") ; Behaviors.same
      }
    }