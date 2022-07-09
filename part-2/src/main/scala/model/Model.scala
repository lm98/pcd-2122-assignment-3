package model

import scala.annotation.targetName
import scala.util.Random

object Costants:
  val defaultHeight: Int = 150
  val defalutWidth: Int = 200

enum ZoneState:
  case Ok
  case Alarm
  case Managing

enum FireStationState:
  case Free
  case Busy

case class RainGauge(zoneID: Int, pos: Point2D)

case class Zone(id: Int, var state: ZoneState, var fireStation: FireStation, bounds: RectangleBounds, var rainGauges: List[RainGauge] = List()):
  def changeState(newState: ZoneState): Unit = state = newState
  def addRainGauge(rainGauge: RainGauge): Unit = rainGauges = rainGauges :+ rainGauge
  def setFireStation(fs: FireStation): Unit = fireStation = fs

case class RectangleBounds(topLeft: Point2D, height: Int = Costants.defaultHeight, width: Int = Costants.defalutWidth):
  def apply(topLeft: Point2D, height: Int, width: Int): RectangleBounds = RectangleBounds(topLeft, height, width)
  def bottomRight: Point2D = Point2D(topLeft.x + width, topLeft.y + height)

case class Point2D(var x: Int = 0, var y: Int = 0):
  def createRandom(tx: Int, ty: Int, bx: Int, by: Int): Point2D =
    val rand = new Random()
    if tx <= ty then
      x = rand.between(tx, ty + 1)
    else
      x = rand.between(ty, tx + 1)
    if bx <= by then
      y = rand.between(bx, by + 1)
    else
      y = rand.between(by, bx + 1)
    Point2D(x, y)

case class FireStation(zoneID: Int, var state: FireStationState, pos: Point2D):
  def changeState(newState: FireStationState): Unit = state = newState