package view

import actors.{ViewActor, ViewActorCommands}
import akka.remote.ContainerFormats.ActorRef
import model.{Costants, RectangleBounds, Zone, ZoneState}

import scala.language.postfixOps
import scala.util.Random

trait View:
  def updateZoneState(zone: Zone, newState: ZoneState): Unit
  def manageZoneClicked(zoneId: Int): Unit

class ViewImpl(var zones: List[Zone]/*, val viewActor: ActorRef[ViewActorCommands]*/) extends View:
  val gui = new AppView(zones) //todo pass view actor
  /*var x = 0
  while x < 1000000000 do x = x + 1
  if x == 1000000000 then
    zones.foreach(z =>
      updateZoneState(z, ZoneState.Alarm)
    )
  while x < 2000000000 do x = x + 1
  if x == 2000000000 then
    zones.foreach(z =>
      updateZoneState(z, ZoneState.Managing)
    )*/

  override def updateZoneState(zone: Zone, newState: ZoneState): Unit =
    zone.changeState(newState)
    gui.updateZone(zone)

  override def manageZoneClicked(zoneId: Int): Unit = ??? /*viewActor ! FixZone(zoneId)*/


@main def testView(): Unit =
  val rand = new Random()
  val rows: Int = 2
  val cols: Int = 3
  var x: Int = 0
  val zones = for
    r <- 0 until rows
    c <- 0 until cols
  yield
    x = x + 1
    new Zone(x, ZoneState.Ok, rand.between(1,4), new RectangleBounds(c *  Costants.defalutWidth, r * Costants.defaultHeight))

  new ViewImpl(zones.toList)

