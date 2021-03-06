package model

import cluster.CborSerializable

import scala.annotation.targetName
import scala.util.Random

object Costants:
  val defaultHeight: Int = 150
  val defalutWidth: Int = 200

enum ZoneState:
  case Ok
  case Alarm
  case Managing

object FireStationState extends Enumeration:
  type FireStationState = Value
  val Free, Busy, Warned = Value

case class RainGauge(zoneID: Int, pos: Point2D)

case class Zone(id: Int, var state: ZoneState, bounds: RectangleBounds):
  def changeState(newState: ZoneState): Unit = state = newState

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

import FireStationState.*
case class FireStation(zoneID: Int, var state: FireStationState, pos: Point2D):
  def changeState(newState: FireStationState): Unit = state = newState