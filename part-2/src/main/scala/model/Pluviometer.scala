package model

enum PluviometerState:
  case Ok
  case Alarm

class Pluviometer(val id: Int, val zoneId: Int, var state: PluviometerState, val x: Int, val y: Int):
  def changeState(newState: PluviometerState): Unit = state = newState