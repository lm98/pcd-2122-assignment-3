package model

import java.awt.geom.Point2D

object Costants:
  val defaultHeight: Int = 150
  val defalutWidth: Int = 200

class Model:
  enum ZoneState:
    case Ok
    case Alarm
    case Managing
  case class RainGauge(zoneID: Int, pos: Point2D)
  case class Zone(id: Int, zoneState: ZoneState, bounds: RectangleBounds):
    var rainGauges: List[RainGauge] = List.empty
    def changeState(newState: ZoneState): Unit = zoneState = newState
    def addRainGauge(rainGauge: RainGauge): Unit = rainGauges = rainGauges :+ rainGauge
  case class RectangleBounds(topLeft: Point2D, height: Int = Costants.defaultHeight, width: Int = Costants.defalutWidth):
    def getBottomRight: Point2D = Point2D(topLeft.getX + width, topLeft.getY + height)
