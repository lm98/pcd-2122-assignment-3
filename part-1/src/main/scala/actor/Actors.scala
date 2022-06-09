package actor


import actor.BodyActor.computeTotalForceOnBody
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.util.Timeout
import conc.model.{Body, Boundary, V2d}

import concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext

case class Request(info: RequestBody, replyTo: ActorRef[Response])

enum RequestBody:
  case UpdateVelocity(body: Body, others: List[Body], dt: Double)
  case UpdatePosition(body: Body, dt: Double)
  case CheckBoundary(body: Body, boundary: Boundary)

case class Response(body: Body)

import actor.RequestBody.*

object MasterActor:
  val dt = 0.001

  def apply(bodies: List[Body],vt: Double = 0.00, i: Int): Behavior[Response] =
    Behaviors setup { ctx =>
      val slave = ctx.spawnAnonymous(SlaveActor())
      given Timeout = 2.seconds
      given Scheduler = ctx.system.scheduler
      given ExecutionContext = ctx.executionContext
      bodies.map(b => slave ? (replyTo => Request(UpdateVelocity(b, bodies, dt), replyTo)))
      MasterActor(bodies, vt + dt, i + 1)
    }


object SlaveActor:
  def computeTotalForceOnBody(body: Body, bodies: List[Body]): V2d =
    val totalForce: V2d = new V2d(0,0)
    bodies.filter(b => !b.equals(body)).foreach(b => {
      val forceOtherBody = body.computeRepulsiveForceBy(b)
      totalForce.sum(forceOtherBody)
    })
    totalForce.sum(body.getCurrentFrictionForce)

  def apply(): Behavior[Request] =
    Behaviors receiveMessage { msg => msg.info match
      case UpdateVelocity(body: Body, others: List[Body], dt: Double) =>
        val totalForce = computeTotalForceOnBody(body, others)
        val acc = new V2d(totalForce).scalarMul(1.0/body.getMass)
        body.updateVelocity(acc, dt) ; msg.replyTo ! Response(body) ; Behaviors.same
      case UpdatePosition(body: Body, dt: Double) =>
        body.updatePos(dt) ; msg.replyTo ! Response(body) ; Behaviors.same
      case CheckBoundary(body: Body, bounds: Boundary) =>
        body.checkAndSolveBoundaryCollision(bounds) ; msg.replyTo ! Response(body) ; Behaviors.stopped
    }