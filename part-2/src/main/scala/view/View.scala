package view

import akka.remote.ContainerFormats.ActorRef
import model.{Costants, RectangleBounds, Zone, ZoneState}

import scala.language.postfixOps
import scala.util.Random

class View(var zones: List[Zone]/*, val viewActor: ActorRef[_]*/):
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

  def updateZoneState(zone: Zone, newState: ZoneState): Unit =
    zone.changeState(newState)
    gui.updateZone(zone)


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

  new View(zones.toList)

