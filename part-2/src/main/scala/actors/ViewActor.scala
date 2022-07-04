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
  case class AlarmOn(/*zone: Zone*/) extends Event with CborSerializable
  case class AlarmOff(/*zone: Zone*/) extends Event with CborSerializable
  case class ManageAlarm(/*zone: Zone*/) extends Event with CborSerializable

  val viewActorServiceKey: ServiceKey[ViewActor.Event] = ServiceKey[ViewActor.Event]("ViewService")

  def apply(zone: Zone): Behavior[ViewActor.Event] =
    Behaviors setup {ctx =>
      val view: AppView = new AppView(zone, ctx.self)
      val subsctiptionAdapter = ctx.messageAdapter[Receptionist.Listing]{
        case FireStationServiceKey.Listing(fireStations) => FireStationsUpdated(fireStations)
      }
      ctx.system.receptionist ! Receptionist.Subscribe(FireStationServiceKey, subsctiptionAdapter)
      ctx.system.receptionist ! Receptionist.Register(viewActorServiceKey, ctx.self)
      running(ctx, IndexedSeq.empty , view, zone)
    }

  private def running(ctx: ActorContext[ViewActor.Event], fireStations: IndexedSeq[ActorRef[FireStation.Event]],  view: AppView, zone: Zone): Behavior[ViewActor.Event] =
    Behaviors receiveMessage { msg => msg match
      case AlarmOn(/*zone*/) =>
        ctx.log.info(s"Zone has alarm on")
        updateZone(zone, ZoneState.Alarm, view)
        running(ctx, fireStations, view, zone)
      case AlarmOff(/*zone*/) =>
        ctx.log.info(s"Zone has alarm off")
        updateZone(zone, ZoneState.Ok, view)
        running(ctx, fireStations, view, zone)
      case FireStationsUpdated(newStations) =>
        ctx.log.info(s"Fire stations have been updated")
        running(ctx, newStations.toIndexedSeq, view, zone)
      case ManageAlarm(/*zone*/) =>
        ctx.log.info(s"Zone is managing alarm")
        fireStations foreach { _ ! FireStation.NotifyAlarmOff()}
        updateZone(zone, ZoneState.Managing, view)
        running(ctx, fireStations, view, zone)
      case _ => Behaviors.same
    }

  private def updateZone(zone: Zone/*, zone: Zone*/, newState: ZoneState, view: AppView): Unit =
    zone.changeState(newState)
//    zones.filter(z => z.id.equals(zone.id)).foreach(u => u.changeState(newState))
    view.updateGui(zone)
