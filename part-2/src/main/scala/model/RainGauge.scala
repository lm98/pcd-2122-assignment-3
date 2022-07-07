package model

enum RainGaugeState:
  case Ok
  case Alarm

class RainGauge(val id: Int, val zoneId: Int, var state: RainGaugeState, val x: Int, val y: Int):
  def changeState(newState: RainGaugeState): Unit = state = newState