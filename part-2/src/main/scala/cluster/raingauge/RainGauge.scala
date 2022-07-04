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
  private case class ZoneRequestRainGaugeToAnother(zone: Int, rainGauge: ActorRef[RainGauge.Event]) extends Event with CborSerializable
  private case class FireStationsUpdated(newSet: Set[ActorRef[FireStation.Event]]) extends Event
  case class ZoneRequestFireStationToRainGauge(fireStation: ActorRef[FireStation.Event]) extends Event with CborSerializable
  private case class Tick() extends Event
  private case class Request(replyTo: ActorRef[Response]) extends Event with CborSerializable
  private case class Response(value: Double) extends Event with CborSerializable
  private case class ReceiveState(state: ResponseState) extends Event with CborSerializable

  val ListenerServiceKey: ServiceKey[RainGauge.Event] = ServiceKey[RainGauge.Event]("RainGauge")
  val ALARM_THRESHOLD = 0.60

  def apply(zone: Int = 0): Behavior[Event] =
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

        running(ctx, Set.empty, Set.empty, 0.0, List.empty, zone)
      }
    }

  private def running(ctx: ActorContext[Event],
                      rainGauges: Set[ActorRef[RainGauge.Event]],
                      fireStations: Set[ActorRef[FireStation.Event]],
                      lastValue: Double,
                      tmpValues: List[ResponseState],
                      zone: Int = 0): Behavior[RainGauge.Event] =

    def checkAlarmCondition(values: List[ResponseState]): Boolean =
      values.count { case Ok(true) => true ; case _ => false } >= (rainGauges.size/2 + 1)

    Behaviors receiveMessage  { msg => msg match
      case RainGaugesUpdated(gauges) =>
        // Ask every other rain gauge if we are in the same zone
        gauges foreach { _ ! ZoneRequestRainGaugeToAnother(zone, ctx.self) }
        Behaviors.same
      case ZoneRequestRainGaugeToAnother(originZone, newRainGauge) =>
        if originZone == zone then
          running(ctx, rainGauges + newRainGauge, fireStations, lastValue, tmpValues, zone)
        else
          running(ctx, rainGauges, fireStations, lastValue, tmpValues, zone)
      case FireStationsUpdated(stations) =>
        stations foreach { _ ! FireStation.ZoneRequestRainGaugeToFireStation(zone, ctx.self) }
        Behaviors.same
      case ZoneRequestFireStationToRainGauge(fireStation) =>
        running(ctx, rainGauges, fireStations + fireStation, lastValue, tmpValues, zone)
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
        running(ctx, rainGauges, fireStations, value, List.empty, zone)
      case Request(replyTo) => replyTo ! Response(lastValue) ; Behaviors.same
      case ReceiveState(state) =>
        val newValues = tmpValues :+ state
        if (checkAlarmCondition(newValues))
          fireStations foreach { _ ! NotifyAlarmOn() }
          running(ctx, rainGauges, fireStations, lastValue, List.empty, zone)
        else
          running(ctx, rainGauges, fireStations, lastValue, newValues, zone)
      case _ => Behaviors.same
    }