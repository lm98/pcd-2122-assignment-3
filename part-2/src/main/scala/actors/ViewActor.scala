package actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import cluster.CborSerializable
import cluster.firestation.FireStation
import cluster.firestation.FireStation.{FireStationServiceKey}
import model.{Costants, RectangleBounds, Zone, ZoneState}
import view.AppView

import scala.util.Random

object ViewActor:
  sealed trait Event
  private case class FireStationsUpdated(newStations: Set[ActorRef[FireStation.Event]]) extends Event
  case class AlarmOn(zoneID: Int) extends Event with CborSerializable
  case class AlarmOff(zoneID: Int) extends Event with CborSerializable
  case class ManageAlarm(zoneID: Int) extends Event with CborSerializable

  val viewActorServiceKey: ServiceKey[ViewActor.Event] = ServiceKey[ViewActor.Event]("ViewService")

  def apply(zones: List[Zone]): Behavior[ViewActor.Event] =
    Behaviors setup {ctx =>
      val view: AppView = new AppView(zones, ctx.self)
      val subscriptionAdapter = ctx.messageAdapter[Receptionist.Listing]{
        case FireStationServiceKey.Listing(fireStations) => FireStationsUpdated(fireStations)
      }
      ctx.system.receptionist ! Receptionist.Subscribe(FireStationServiceKey, subscriptionAdapter)
      ctx.system.receptionist ! Receptionist.Register(viewActorServiceKey, ctx.self)
      running(ctx, IndexedSeq.empty , view, zones)
    }

  private def running(ctx: ActorContext[ViewActor.Event], fireStations: IndexedSeq[ActorRef[FireStation.Event]],  view: AppView, zones: List[Zone]): Behavior[ViewActor.Event] =
    Behaviors receiveMessage { msg => msg match
      case AlarmOn(zoneID) =>
        ctx.log.info(s"Zone $zoneID has alarm on")
        updateZone(zoneID, ZoneState.Alarm, view, zones)
        running(ctx, fireStations, view, zones)
      case AlarmOff(zoneID) =>
        ctx.log.info(s"Zone $zoneID has alarm off")
        updateZone(zoneID, ZoneState.Ok, view, zones)
        running(ctx, fireStations, view, zones)
      case FireStationsUpdated(newStations) =>
        ctx.log.info(s"Fire stations have been updated")
        running(ctx, newStations.toIndexedSeq, view, zones)
      case ManageAlarm(zoneID) =>
        ctx.log.info(s"Zone $zoneID is managing alarm")
        fireStations foreach { _ ! FireStation.NotifyAlarmOff()}
        updateZone(zoneID, ZoneState.Managing, view, zones)
        running(ctx, fireStations, view, zones)
      case _ => Behaviors.same
    }

  private def updateZone(zoneID: Int, newState: ZoneState, view: AppView, zones: List[Zone]): Unit =
//    zone.changeState(newState)
    zones.filter(z => z.id.equals(zoneID)).foreach(u => u.changeState(newState))
    view.updateGui(zones)

