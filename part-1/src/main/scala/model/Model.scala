package model

import model.Objects2d.*

import scala.annotation.targetName

case class Body(id: Int, pos: P2d, vel: V2d, mass: Double):
  val REPULSIVE_CONST = 0.01
  val FRICTION_CONST = 1.00

case class Boundary(x0: Double, y0: Double, x1: Double, y1: Double)

object Objects2d:
  case class V2d(x: Double, y: Double)
  object V2d:
    def apply(x: Double, y: Double): V2d = V2d(x, y)
    def apply(v2d: V2d): V2d = v2d
    def apply(from: P2d, to: P2d): V2d = V2d(from.x - to.x, from.y - to.y)
  extension (v: V2d)
    @targetName("scalarMul")
    def :*(k: Double): V2d = V2d(v.x * k, v.y * k)
    @targetName("sum")
    def +(v2d: V2d): V2d = V2d(v.x + v2d.x, v.y + v2d.y)

  case class P2d(x: Double, y: Double)
  extension (p: P2d)
    @targetName("sum")
    def +(p2d: P2d): P2d = P2d(p.x + p2d.x, p.y + p2d.y)