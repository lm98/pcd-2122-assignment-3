package model

object Costants:
  val defaultHeight: Int = 150
  val defalutWidth: Int = 200

class Model:
  enum ZoneState:
    case Ok
    case Alarm
    case Managing
  enum FireStationState:
    case Free
    case Occupied
  case class RainGauge(zoneID: Int, pos: Point2D)
  case class Zone(id: Int, var zoneState: ZoneState, var fireStation: FireStation, bounds: RectangleBounds):
    var rainGauges: List[RainGauge] = List.empty
    def changeState(newState: ZoneState): Unit = zoneState = newState
    def addRainGauge(rainGauge: RainGauge): Unit = rainGauges = rainGauges :+ rainGauge
    def setFireStation(fs: FireStation): Unit = fireStation = fs
  case class RectangleBounds(topLeft: Point2D, height: Int = Costants.defaultHeight, width: Int = Costants.defalutWidth):
    def getBottomRight: Point2D = Point2D(topLeft.x + width, topLeft.y + height)
  case class Point2D(x: Int, y: Int)
  case class FireStation(zoneID: Int, var fireStationState: FireStationState, pos: Point2D)
    def changeState(newState: FireStationState): Unit = fireStationState = newState