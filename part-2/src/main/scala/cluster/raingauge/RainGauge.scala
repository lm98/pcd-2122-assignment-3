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

enum ResponseState:
  case Ok(isAlarmed: Boolean)
  case Unreachable

import ResponseState.*

object RainGauge:
  sealed trait Event
  private case class RainGaugesUpdated(newSet: Set[ActorRef[Event]]) extends Event
  private case class FireStationsUpdated(newSet: Set[ActorRef[NotifyAlarmOn]]) extends Event
  private case class Tick() extends Event
  private case class Request(replyTo: ActorRef[Response]) extends Event with CborSerializable
  private case class Response(value: Double) extends Event with CborSerializable
  private case class ReceiveState(state: ResponseState) extends Event with CborSerializable

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

        timers.startTimerWithFixedDelay(Tick(), Tick(), 5.seconds)

        running(ctx, IndexedSeq.empty, IndexedSeq.empty, 0.0, IndexedSeq.empty)
      }
    }

  private def running(ctx: ActorContext[RainGauge.Event],
                      rainGauges: IndexedSeq[ActorRef[RainGauge.Event]],
                      fireStations:IndexedSeq[ActorRef[NotifyAlarmOn]],
                      lastValue: Double,
                      tmpValues: IndexedSeq[ResponseState]): Behavior[RainGauge.Event] =

    def checkAlarmCondition(values: IndexedSeq[ResponseState]): Boolean =
      values.count { case Ok(true) => true ; case _ => false } >= (rainGauges.size/2 + 1)

    Behaviors receiveMessage  { msg => msg match
      case RainGaugesUpdated(gauges) =>
        running(ctx, gauges.toIndexedSeq, fireStations, lastValue, tmpValues)
      case FireStationsUpdated(newSet) =>
        running(ctx, rainGauges, newSet.toIndexedSeq, lastValue, tmpValues)
      case Tick() =>
        //Check other rain gauges values
        given Timeout = 2.seconds
        rainGauges.foreach(l => ctx.ask(l, Request.apply){
          case Success(Response(value)) => ReceiveState(Ok(value >= ALARM_THRESHOLD))
          case _ => ReceiveState(Unreachable)
        })
        //Update the value
        val value = ThreadLocalRandom.current().nextDouble()
        ctx.log.info(s"${ctx.self.path.name}: Produced $value")
        running(ctx, rainGauges, fireStations, value, IndexedSeq.empty)
      case Request(replyTo) => replyTo ! Response(lastValue) ; Behaviors.same
      case ReceiveState(state) =>
        val newValues = tmpValues :+ state
        if (checkAlarmCondition(newValues))
          fireStations foreach { _ ! NotifyAlarmOn() }
          running(ctx, rainGauges, fireStations, lastValue, IndexedSeq.empty)
        else
          running(ctx, rainGauges, fireStations, lastValue, newValues)
      case _ => Behaviors.same
    }