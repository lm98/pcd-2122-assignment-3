package model

enum ZoneState:
  case Ok
  case Alarm
  case Managing

object Constants:
  val defaultHeight = 150
  val defaultWidth = 200

class Zone(val id: Int, var state: ZoneState, val numDevices: Int, val height: Int = Constants.defaultHeight, val width: Int = Constants.defaultWidth):
  def changeState(newState: ZoneState): Zone = Zone(id, newState, numDevices, height, width)
