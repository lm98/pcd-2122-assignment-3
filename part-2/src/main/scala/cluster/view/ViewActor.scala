package cluster.view

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import cluster.CborSerializable
import cluster.firestation.FireStationActor
import cluster.firestation.FireStationActor.FireStationServiceKey
import cluster.raingauge.RainGaugeActor
import model.*
import view.AppView

import scala.collection.immutable.IndexedSeq
import scala.util.Random

object ViewActor:
  sealed trait Event

  private case class FireStationsUpdated(newStations: Set[ActorRef[FireStationActor.Event]]) extends Event
  case class AlarmOn(zoneID: Int) extends Event with CborSerializable
  case class AlarmOff(zoneID: Int) extends Event with CborSerializable
  case class ManageAlarm(zoneID: Int) extends Event with CborSerializable
  case class RainGaugesUpdated(newSet: Set[ActorRef[RainGaugeActor.Event]]) extends Event with CborSerializable
  case class AddRainGauge(rainGauge: RainGauge) extends Event with CborSerializable
  case class AddFireStation(fireStation: FireStation) extends Event with CborSerializable
  case class UpdateStation(fireStation: FireStation) extends Event with CborSerializable

  val ViewActorServiceKey: ServiceKey[ViewActor.Event] = ServiceKey[ViewActor.Event]("ViewService")
  var zoneList: List[Zone] = List()
  def apply(zones: List[Zone]): Behavior[ViewActor.Event] =
    Behaviors setup { ctx =>
      zoneList = zones
      val view = AppView(zoneList, List.empty, List.empty, ctx.self)
      val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
        case FireStationServiceKey.Listing(fireStations) => FireStationsUpdated(fireStations)
        case RainGaugeActor.RainGaugeServiceKey.Listing(newGauge) => RainGaugesUpdated(newGauge)
      }
      ctx.system.receptionist ! Receptionist.Subscribe(FireStationServiceKey, subscriptionAdapter)
      ctx.system.receptionist ! Receptionist.Subscribe(RainGaugeActor.RainGaugeServiceKey, subscriptionAdapter)
      ctx.system.receptionist ! Receptionist.Register(ViewActorServiceKey, ctx.self)
      running(ctx, IndexedSeq.empty, IndexedSeq.empty, view)
    }

  private def running(ctx: ActorContext[ViewActor.Event], fireStations: IndexedSeq[ActorRef[FireStationActor.Event]], rainGauges: IndexedSeq[ActorRef[RainGaugeActor.Event]], view: AppView): Behavior[ViewActor.Event] =
    Behaviors receiveMessage { msg =>
      msg match
        case RainGaugesUpdated(rainGauge) =>
          ctx.log.info(s"Rain gauges have been updated to ${rainGauge.size}")
          running(ctx, fireStations, rainGauge.toIndexedSeq, view)
        case FireStationsUpdated(newStations) =>
          ctx.log.info(s"Fire stations have been updated to ${newStations.size}")
          running(ctx, newStations.toIndexedSeq, rainGauges, view)
        case AlarmOn(zoneID) =>
          ctx.log.info(s"Zone $zoneID has alarm on")
          updateZone(zoneID, ZoneState.Alarm, view)
          running(ctx, fireStations, rainGauges, view)
        case AlarmOff(zoneID) =>
          ctx.log.info(s"Zone $zoneID has alarm off")
          updateZone(zoneID, ZoneState.Ok, view)
          running(ctx, fireStations, rainGauges, view)
        case ManageAlarm(zoneID) =>
          ctx.log.info(s"Zone $zoneID is managing alarm")
          fireStations foreach { _ ! FireStationActor.ManageAlarm(zoneID) }
          updateZone(zoneID, ZoneState.Managing, view)
          running(ctx, fireStations, rainGauges, view)
        case AddRainGauge(newGauge) =>
          ctx.log.info(s"Added rain gauge to zone ${newGauge.zoneID}")
          view.updateRainGauges(newGauge)
          running(ctx, fireStations, rainGauges, view)
        case AddFireStation(newStation) =>
          ctx.log.info(s"Added firestation to zone ${newStation.zoneID}")
          view.updateStations(newStation)
          running(ctx, fireStations, rainGauges, view)
        case UpdateStation(fireStation) =>
          ctx.log.info(s"Fire station ${fireStation.zoneID} is Busy")
          view.updateFireStationsState(fireStation)
          running(ctx, fireStations, rainGauges, view)
    }

  private def updateZone(zoneID: Int, newState: ZoneState, view: AppView): Unit =
    zoneList.filter(z => z.id.equals(zoneID)).foreach(u => u.changeState(newState))
    view.updateGui(zoneList)
