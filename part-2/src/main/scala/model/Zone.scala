package model

import scala.util.Random

enum ZoneState:
  case Ok
  case Alarm
  case Managing

class Zone(val id: Int, var state: ZoneState, val numDevices: Int, val bounds: RectangleBounds):
  val rand = new Random()
  var pluviometers = for x <- 0 until numDevices yield
    new Pluviometer(x + 1, id, PluviometerState.Ok, rand.between(bounds.x0 + 10, bounds.getX1 - 10), rand.between(bounds.y0 + 10, bounds.getY1 - 10))

  def changeState(newState: ZoneState): Unit = state = newState
