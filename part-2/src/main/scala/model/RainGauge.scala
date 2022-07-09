package model

import java.awt.geom.Point2D

class RainGauge(val zoneId: Int):
  var pos: Point2D = new Point2D:
    override def getX: Double = pos.getX

    override def getY: Double = pos.getY

    override def setLocation(x: Double, y: Double): Unit = pos.setLocation (x, y)

