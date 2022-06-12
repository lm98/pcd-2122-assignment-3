package model

import conc.model.exc.{InfiniteForceException, NullVectorException}
import model.Objects2d.*
import model.Objects2d.V2d.normalize

import scala.annotation.{tailrec, targetName}

case class Boundary(x0: Double, y0: Double, x1: Double, y1: Double)

case class Body(id: Int, pos: P2d, vel: V2d, mass: Double)


object Body:
  val REPULSIVE_CONST = 0.01
  val FRICTION_CONST = 1.00

  def updatePos(body:Body, dt: Double): Body = body match
    case Body(id, pos, vel, mass) => Body(id, pos + (vel :* dt), vel, mass)

  def updateVelocity(body: Body, acc: V2d, dt: Double): Body = body match
    case Body(id, pos, vel, mass) => Body(id, pos, vel + (acc :* dt), mass)

  def getDistanceFrom(from: Body, to: Body): Double =
    val dx = from.pos.x - to.pos.x
    val dy = from.pos.y - to.pos.y
    Math.sqrt(dx*dx + dy*dy)

  def computeRepulsiveForceBy(to: Body, by: Body): V2d =
    val dist = getDistanceFrom(to, by)
    if dist < 0 then throw new InfiniteForceException
    try
      normalize(V2d(by.pos,to.pos)) :* (by.mass * REPULSIVE_CONST / (dist*dist))
    catch
      case _: Exception => throw new InfiniteForceException

  def getCurrentFrictionForce(body: Body): V2d =
    body.vel :* (- FRICTION_CONST)

  def checkAndSolveBoundaryCollision(body: Body, boundary: Boundary): Body =
    val x = body.pos.x ; val y = body.pos.y
    (x, y) match
      case _ if x > boundary.x1 =>
        Body(body.id, P2d(boundary.x1, y), V2d(-body.vel.x, body.vel.y), body.mass)
      case _ if x < boundary.x0 =>
        Body(body.id, P2d(boundary.x0, y), V2d(-body.vel.x, body.vel.y), body.mass)
      case _ if y > boundary.y1 =>
        Body(body.id, P2d(x, boundary.y1), V2d(body.vel.x, -body.vel.y), body.mass)
      case _ if y < boundary.y0 =>
        Body(body.id, P2d(x, boundary.y0), V2d(body.vel.x, -body.vel.y), body.mass)


object Objects2d:
  case class V2d(x: Double, y: Double)
  object V2d:
    @tailrec
    def apply(x: Double, y: Double): V2d = V2d(x, y)
    def apply(v2d: V2d): V2d = v2d
    def apply(from: P2d, to: P2d): V2d = V2d(from.x - to.x, from.y - to.y)
    def normalize(v2d: V2d): V2d = v2d match
      case V2d(x,y) =>
        val mod = Math.sqrt(v2d.x*v2d.x + v2d.y*v2d.y)
        if mod > 0 then
          V2d(v2d.x / mod, v2d.y / mod)
        else throw new NullVectorException

  extension (v: V2d)
    @targetName("scalarMul")
    def :*(k: Double): V2d = V2d(v.x * k, v.y * k)
    @targetName("sum")
    def +(v2d: V2d): V2d = V2d(v.x + v2d.x, v.y + v2d.y)

  case class P2d(x: Double, y: Double)
  extension (p: P2d)
    @targetName("sum")
    def +(v2d: V2d): P2d = P2d(p.x + v2d.x, p.y + v2d.y)