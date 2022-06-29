package model

object Costants:
  val defaultHeight: Int = 150
  val defalutWidth: Int = 200

class RectangleBounds(val x0: Int, val y0: Int, val height: Int = Costants.defaultHeight, val width: Int = Costants.defalutWidth):
  def apply(x0: Int, y0: Int, h: Int, w: Int): RectangleBounds = new RectangleBounds(x0, y0, h, w)
  def getY1: Int = y0 + height
  def getX1: Int = x0 + width

