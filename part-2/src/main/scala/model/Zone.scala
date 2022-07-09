package model

import scala.util.Random
import model.RainGauge


enum ZoneState:
  case Ok
  case Alarm
  case Managing

class Zone(val id: Int, var state: ZoneState, val bounds: RectangleBounds):
  var rainGauges: List[RainGauge] = List().empty
  def changeState(newState: ZoneState): Unit = state = newState
  def addRainGauge(rainGauge: RainGauge): Unit =
    rainGauges = rainGauges :+ rainGauge
