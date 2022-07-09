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
  case class AddRainGauge(/*rainGauge: RainGauge*/) extends Event with CborSerializable
  case class RainGaugesUpdated(newSet: Set[ActorRef[RainGaugeActor.Event]]) extends Event with CborSerializable
  private case class ViewUpdated(newSet: Set[ActorRef[Event]]) extends Event with CborSerializable
  case class AddFireStation() extends Event with CborSerializable
  case class FireStationBusy(zoneID: Int) extends Event with CborSerializable
  case class FireStationFree(zoneID: Int) extends Event with CborSerializable

  val ViewActorServiceKey: ServiceKey[ViewActor.Event] = ServiceKey[ViewActor.Event]("ViewService")
  var zoneList: List[Zone] = List()
  var viewList: List[AppView] = List()
  def apply(zones: List[Zone]): Behavior[ViewActor.Event] =
    Behaviors setup { ctx =>
      zoneList = zones
      viewList = viewList :+ AppView(zoneList, ctx.self)
      val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing] {
        case FireStationServiceKey.Listing(fireStations) => FireStationsUpdated(fireStations)
        case RainGaugeActor.ListenerServiceKey.Listing(newGauge) => RainGaugesUpdated(newGauge)
        case ViewActor.ViewActorServiceKey.Listing(newView) => ViewUpdated(newView)
      }
      ctx.system.receptionist ! Receptionist.Subscribe(FireStationServiceKey, subscriptionAdapter)
      ctx.system.receptionist ! Receptionist.Subscribe(RainGaugeActor.ListenerServiceKey, subscriptionAdapter)
      ctx.system.receptionist ! Receptionist.Register(ViewActorServiceKey, ctx.self)
      running(ctx, IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty)
    }

  private def running(ctx: ActorContext[ViewActor.Event], fireStations: IndexedSeq[ActorRef[FireStationActor.Event]], rainGauges: IndexedSeq[ActorRef[RainGaugeActor.Event]], views: IndexedSeq[ActorRef[ViewActor.Event]]): Behavior[ViewActor.Event] =
    Behaviors receiveMessage { msg =>
      msg match
        case ViewUpdated(newView) =>
          ctx.log.info(s"Views have been updated")
          running(ctx, fireStations, rainGauges, newView.toIndexedSeq)
        case RainGaugesUpdated(rainGauge) =>
          ctx.log.info(s"Rain gauges have been updated")
          running(ctx, fireStations, rainGauge.toIndexedSeq, views)
        case FireStationsUpdated(newStations) =>
          ctx.log.info(s"Fire stations have been updated")
          running(ctx, newStations.toIndexedSeq, rainGauges, views)
        case AlarmOn(zoneID) =>
          ctx.log.info(s"Zone $zoneID has alarm on")
          updateZone(zoneID, ZoneState.Alarm)
          running(ctx, fireStations, rainGauges, views)
        case AlarmOff(zoneID) =>
          ctx.log.info(s"Zone $zoneID has alarm off")
          updateZone(zoneID, ZoneState.Ok)
          running(ctx, fireStations, rainGauges, views)
        case ManageAlarm(zoneID) =>
          ctx.log.info(s"Zone $zoneID is managing alarm")
          fireStations foreach { _ ! FireStationActor.ManageAlarm() }
          updateZone(zoneID, ZoneState.Managing)
          running(ctx, fireStations, rainGauges, views)
        case AddRainGauge() =>
          ctx.log.info(" ==== Add rain gauge ==== ")
          running(ctx, fireStations, rainGauges, views)
        case FireStationBusy(zoneID) => 
          ctx.log.info(s"Fire station $zoneID is Busy")
          updateFireStation(zoneID, FireStationState.Busy)
          running(ctx, fireStations, rainGauges, views)
        case FireStationFree(zoneID) =>
          ctx.log.info(s"Fire station $zoneID is free")
          updateFireStation(zoneID, FireStationState.Free)
          running(ctx, fireStations, rainGauges, views)
    }

  private def updateZone(zoneID: Int, newState: ZoneState): Unit =
    zoneList.filter(z => z.id.equals(zoneID)).foreach(u => u.changeState(newState))
    viewList.foreach(v => v.updateGui(zoneList))

  private def updateFireStation(zoneID: Int, newState: FireStationState): Unit =
    zoneList.filter(z => z.id.equals(zoneID)).foreach(u => u.fireStation.changeState(newState))
    viewList.foreach(v => v.updateGui(zoneList))
