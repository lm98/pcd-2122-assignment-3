package cluster.raingauge

import akka.actor.typed.{ActorRef, Behavior, Scheduler}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.util.Timeout
import cluster.CborSerializable
import cluster.firestation.FireStationActor
import cluster.firestation.FireStationActor.{FireStationServiceKey, NotifyAlarmOn}
import cluster.view.ViewActor
import model.*

import java.util.concurrent.ThreadLocalRandom
import concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success}

enum ResponseState:
  case Ok(isAlarmed: Boolean)
  case Unreachable

import ResponseState.*

object RainGaugeActor:
  sealed trait Event
  private case class RainGaugesUpdated(newSet: Set[ActorRef[Event]]) extends Event
  private case class ZoneRequestRainGaugeToAnother(zone: Int, rainGauge: ActorRef[RainGaugeActor.Event]) extends Event with CborSerializable
  private case class FireStationsUpdated(newSet: Set[ActorRef[FireStationActor.Event]]) extends Event
  case class ZoneRequestFireStationToRainGauge(fireStation: ActorRef[FireStationActor.Event]) extends Event with CborSerializable
  private case class Tick() extends Event
  private case class Request(replyTo: ActorRef[Response]) extends Event with CborSerializable
  private case class Response(value: Double) extends Event with CborSerializable
  private case class ReceiveState(state: ResponseState) extends Event with CborSerializable
  private case class ViewUpdated(value: Set[ActorRef[ViewActor.Event]]) extends Event with CborSerializable

  val RainGaugeServiceKey: ServiceKey[RainGaugeActor.Event] = ServiceKey[RainGaugeActor.Event]("RainGauge")
  val ALARM_THRESHOLD = 0.80

  def apply(rainGauge: RainGauge): Behavior[Event] =
    Behaviors setup { ctx =>
      Behaviors withTimers { timers =>
        val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
          case RainGaugeActor.RainGaugeServiceKey.Listing(newGauges) =>
            RainGaugesUpdated(newGauges)
          case FireStationActor.FireStationServiceKey.Listing(newFireStations) =>
            FireStationsUpdated(newFireStations)
          case ViewActor.ViewActorServiceKey.Listing(newView) => ViewUpdated(newView)
        }
        ctx.system.receptionist ! Receptionist.Subscribe(RainGaugeServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Subscribe(FireStationServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Subscribe(ViewActor.ViewActorServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Register(RainGaugeServiceKey, ctx.self)

        timers.startTimerWithFixedDelay(Tick(), Tick(), 2.seconds)

        running(ctx, Set.empty, Set.empty, 0.0, List.empty, rainGauge)
      }
    }

  private def running(ctx: ActorContext[Event],
                      rainGauges: Set[ActorRef[RainGaugeActor.Event]],
                      fireStations: Set[ActorRef[FireStationActor.Event]],
                      lastValue: Double,
                      tmpValues: List[ResponseState],
                      rainGauge: RainGauge): Behavior[RainGaugeActor.Event] =

    def checkAlarmCondition(values: List[ResponseState]): Boolean =
      values.count { case Ok(true) => true ; case _ => false } >= (rainGauges.size/2 + 1)

    Behaviors receiveMessage  { msg => msg match
      case ViewUpdated(newSet) =>
        newSet foreach { _ ! ViewActor.AddRainGauge(rainGauge)}
        ctx.log.info(s"Views have been updated to ${newSet.size}")
        running(ctx, rainGauges, fireStations, lastValue, tmpValues, rainGauge)
      case RainGaugesUpdated(gauges) =>
        // Ask every other rain gauge if we are in the same zone
        gauges foreach { _ ! ZoneRequestRainGaugeToAnother(rainGauge.zoneID, ctx.self) }
        Behaviors.same
      case ZoneRequestRainGaugeToAnother(originZone, newRainGauge) =>
        if originZone == rainGauge.zoneID then
          running(ctx, rainGauges + newRainGauge, fireStations, lastValue, tmpValues, rainGauge)
        else
          running(ctx, rainGauges, fireStations, lastValue, tmpValues, rainGauge)
      case FireStationsUpdated(stations) =>
        stations foreach { _ ! FireStationActor.ZoneRequestRainGaugeToFireStation(rainGauge.zoneID, ctx.self) }
        Behaviors.same
      case ZoneRequestFireStationToRainGauge(fireStation) =>
        running(ctx, rainGauges, fireStations + fireStation, lastValue, tmpValues, rainGauge)
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
        running(ctx, rainGauges, fireStations, value, List.empty, rainGauge)
      case Request(replyTo) => replyTo ! Response(lastValue) ; Behaviors.same
      case ReceiveState(state) =>
        val newValues = tmpValues :+ state
        if (checkAlarmCondition(newValues))
          fireStations foreach { _ ! NotifyAlarmOn() }
          running(ctx, rainGauges, fireStations, lastValue, List.empty, rainGauge)
        else
          running(ctx, rainGauges, fireStations, lastValue, newValues, rainGauge)
      case _ => Behaviors.same
    }