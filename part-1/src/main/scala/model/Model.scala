package model

import model.Objects2d.*
import model.Objects2d.V2dOp.normalize
import scala.annotation.{tailrec, targetName}

class InfiniteForceException extends Exception
class NullVectorException extends Exception

case class Boundary(x0: Double, y0: Double, x1: Double, y1: Double)

case class Body(id: Int, pos: P2d, vel: V2d, mass: Double)

case object BodyOp:
  val REPULSIVE_CONST = 0.01
  val FRICTION_CONST = 1.00

  def updatePos(body: Body, dt: Double): Body = body match
    case Body(id, pos, vel, mass) => Body(id, pos :+ (vel :* dt), vel, mass)

  def updateVelocity(body: Body, acc: V2d, dt: Double): Body = body match
    case Body(id, pos, vel, mass) => Body(id, pos, vel + (acc :* dt), mass)

  def changeVel(body: Body, v2d: V2d): Body = body match
    case Body(id, pos, _, mass) => Body(id, pos, v2d, mass)

  def getDistanceFrom(body: Body, from: Body): Double =
    val dx = body.pos.x - from.pos.x
    val dy = body.pos.y - from.pos.y
    Math.sqrt(dx*dx + dy*dy)

  def computeRepulsiveForceBy(to: Body, by: Body): V2d =
    val dist = getDistanceFrom(to, by)
    if dist <= 0 then throw new InfiniteForceException else
      try
        normalize(V2dOp(by.pos,to.pos)) :* (by.mass * REPULSIVE_CONST / (dist*dist))
      catch
        case _: Exception => throw new InfiniteForceException

  def getCurrentFrictionForce(body: Body): V2d =
    body.vel :* (- FRICTION_CONST)

  def computeTotalForceOnBody(body: Body, bodies: List[Body]): V2d =
    bodies.filterNot(b => b.equals(body)).foldLeft(V2d(0,0))((acc, b) => acc + computeRepulsiveForceBy(body, b))

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
      case _ => body

object Objects2d:
  case class V2d(x: Double, y: Double)
  object V2dOp:
    def apply(x: Double, y: Double): V2d = V2d(x, y)
    def apply(v2d: V2d): V2d = v2d
    def apply(from: P2d, to: P2d): V2d = V2d(to.x - from.x,to.y - from.y)
    def normalize(v2d: V2d): V2d = v2d match
      case V2d(x,y) =>
        val mod = Math.sqrt(x*x + y*y)
        if mod > 0 then
          V2d(x / mod, y / mod)
        else throw new NullVectorException
  extension (v: V2d)
    @targetName("scalarMul")
    def :*(k: Double): V2d = V2d(v.x * k, v.y * k)
    @targetName("sum")
    def +(v2d: V2d): V2d = V2d(v.x + v2d.x, v.y + v2d.y)

  case class P2d(x: Double, y: Double)
  object P2dOp:
    def apply(x: Int, y: Int): P2d = P2d(x, y)
  extension (p: P2d)
    @targetName("sumVec")
    def :+(v2d: V2d): P2d = P2d(p.x + v2d.x, p.y + v2d.y)
    @targetName("sum")
    def +(p2d: P2d): P2d = P2d(p.x + p2d.x, p.y + p2d.y)