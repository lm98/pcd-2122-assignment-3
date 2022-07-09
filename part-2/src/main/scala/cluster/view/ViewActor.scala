package cluster.view

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import cluster.CborSerializable
import cluster.firestation.FireStationActor
import cluster.firestation.FireStationActor.FireStationServiceKey
import cluster.raingauge.RainGaugeActor
import model.{RainGauge, RectangleBounds, Zone, ZoneState}
import view.AppView

import scala.collection.immutable.IndexedSeq
import scala.util.Random

object ViewActor:
  sealed trait Event

  private case class FireStationsUpdated(newStations: Set[ActorRef[FireStationActor.Event]]) extends Event
  private case class AlarmOn(zoneID: Int) extends Event with CborSerializable
  private case class AlarmOff(zoneID: Int) extends Event with CborSerializable
  private case class ManageAlarm(zoneID: Int) extends Event with CborSerializable
  private case class AddRainGauge(rainGauge: RainGauge) extends Event with CborSerializable
  private case class RainGaugesUpdated(newSet: Set[ActorRef[RainGaugeActor.Event]]) extends Event with CborSerializable
  private case class ViewUpdated(newSet: Set[ActorRef[Event]]) extends Event with CborSerializable

  case class AddFireStation()

  val ViewActorServiceKey: ServiceKey[ViewActor.Event] = ServiceKey[ViewActor.Event]("ViewService")
  var zoneList: List[Zone] = List()
  val viewList: List[AppView] = List()
  def apply(zones: List[Zone]): Behavior[ViewActor.Event] =
    Behaviors setup { ctx =>
      zoneList = zones
      viewList :+ new AppView(zones, ctx.self)
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
        case AddRainGauge(rainGauge) =>
          ctx.log.info(s" === ADD Rain gauges ===") //TODO DOES NOT ENTER
          setRainGaugesBounds(rainGauge)
          addRaingGaugeToZone(rainGauge)
          updateZone(rainGauge.zoneId, ZoneState.Ok, views)
          running(ctx, fireStations, rainGauges, views)
        case AddFireStation() => ???
        case AlarmOn(zoneID) =>
          ctx.log.info(s"Zone $zoneID has alarm on")
          updateZone(zoneID, ZoneState.Alarm, views)
          running(ctx, fireStations, rainGauges, views)
        case AlarmOff(zoneID) =>
          ctx.log.info(s"Zone $zoneID has alarm off")
          updateZone(zoneID, ZoneState.Ok, views)
          running(ctx, fireStations, rainGauges, views)
        case ManageAlarm(zoneID) =>
          ctx.log.info(s"Zone $zoneID is managing alarm")
          fireStations foreach {
            _ ! FireStationActor.NotifyAlarmOff()
          }
          updateZone(zoneID, ZoneState.Managing, views)
          running(ctx, fireStations, rainGauges, views)
        case _ => Behaviors.same
    }

  private def updateZone(zoneID: Int, newState: ZoneState, views: IndexedSeq[ActorRef[ViewActor.Event]]): Unit =
    zoneList.filter(z => z.id.equals(zoneID)).foreach(u => u.changeState(newState))
    viewList.foreach(v => v.updateGui(zoneList))

  private def setRainGaugesBounds(rainGauge: RainGauge): Unit =
    var bounds: RectangleBounds = new RectangleBounds(0, 0)
    val rand = new Random()
    zoneList.filter(z => z.id.equals(rainGauge.zoneId)).foreach(z => bounds = z.bounds)
    rainGauge.pos.setLocation(rand.between(bounds.x0 + 10, bounds.getX1 - 10), rand.between(bounds.y0 + 10, bounds.getY1 - 10))

  private def addRaingGaugeToZone(rainGauge: RainGauge): Unit =
    zoneList.filter(z => z.id.equals(rainGauge.zoneId)).foreach(z => z.addRainGauge(rainGauge))
