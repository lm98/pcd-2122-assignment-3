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
  private case class SendNewRainGauge(newSet: Set[ActorRef[Event]]) extends Event with CborSerializable
  private case class ViewUpdated(value: Set[ActorRef[ViewActor.Event]]) extends Event with CborSerializable

  val ListenerServiceKey: ServiceKey[RainGaugeActor.Event] = ServiceKey[RainGaugeActor.Event]("RainGauge")
  val ALARM_THRESHOLD = 0.60

  def apply(zone: Int = 1): Behavior[Event] =
    Behaviors setup { ctx =>
      Behaviors withTimers { timers =>
        val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
          case RainGaugeActor.ListenerServiceKey.Listing(newGauges) =>
            RainGaugesUpdated(newGauges)
          case FireStationActor.FireStationServiceKey.Listing(newFireStations) =>
            FireStationsUpdated(newFireStations)
          case ViewActor.ViewActorServiceKey.Listing(newView) => ViewUpdated(newView)
        }
        ctx.system.receptionist ! Receptionist.Subscribe(ListenerServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Subscribe(FireStationServiceKey, subscriptionAdapter)
        ctx.system.receptionist ! Receptionist.Register(ListenerServiceKey, ctx.self)

        timers.startTimerWithFixedDelay(Tick(), Tick(), 2.seconds)

        running(ctx, Set.empty, Set.empty, Set.empty, 0.0, List.empty, zone)
      }
    }

  private def running(ctx: ActorContext[Event],
                      rainGauges: Set[ActorRef[RainGaugeActor.Event]],
                      fireStations: Set[ActorRef[FireStationActor.Event]],
                      views: Set[ActorRef[ViewActor.Event]],
                      lastValue: Double,
                      tmpValues: List[ResponseState],
                      zone: Int = 0): Behavior[RainGaugeActor.Event] =

    def checkAlarmCondition(values: List[ResponseState]): Boolean =
      values.count { case Ok(true) => true ; case _ => false } >= (rainGauges.size/2 + 1)

    Behaviors receiveMessage  { msg => msg match
      case ViewUpdated(views) => 
        views foreach { _ => ???}
        Behaviors.same
      case RainGaugesUpdated(gauges) =>
        // Ask every other rain gauge if we are in the same zone
        gauges foreach { _ ! ZoneRequestRainGaugeToAnother(zone, ctx.self) }
        Behaviors.same
      case SendNewRainGauge(newSet) =>
        views foreach { _ => ViewActor.AddRainGauge()}
        running(ctx, newSet, fireStations, views, lastValue, tmpValues, zone)
      case ZoneRequestRainGaugeToAnother(originZone, newRainGauge) =>
        if originZone == zone then
          running(ctx, rainGauges + newRainGauge, fireStations, views, lastValue, tmpValues, zone)
        else
          running(ctx, rainGauges, fireStations, views, lastValue, tmpValues, zone)
      case FireStationsUpdated(stations) =>
        stations foreach { _ ! FireStationActor.ZoneRequestRainGaugeToFireStation(zone, ctx.self) }
        Behaviors.same
      case ZoneRequestFireStationToRainGauge(fireStation) =>
        running(ctx, rainGauges, fireStations + fireStation, views, lastValue, tmpValues, zone)
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
        running(ctx, rainGauges, fireStations, views, value, List.empty, zone)
      case Request(replyTo) => replyTo ! Response(lastValue) ; Behaviors.same
      case ReceiveState(state) =>
        val newValues = tmpValues :+ state
        if (checkAlarmCondition(newValues))
          fireStations foreach { _ ! NotifyAlarmOn() }
          running(ctx, rainGauges, fireStations, views, lastValue, List.empty, zone)
        else
          running(ctx, rainGauges, fireStations, views, lastValue, newValues, zone)
      case _ => Behaviors.same
    }