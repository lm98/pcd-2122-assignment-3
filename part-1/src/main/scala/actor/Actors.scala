package actor


import actor.BodyActor.computeTotalForceOnBody
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.util.Timeout
import conc.model.{Body, Boundary, V2d}
import jdk.javadoc.doclet.Reporter

import concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

case class Request(info: RequestBody, replyTo: ActorRef[Response])

enum RequestBody:
  case UpdateVelocity(body: Body, others: List[Body], dt: Double)
  case UpdatePosition(body: Body, dt: Double)
  case CheckBoundary(body: Body, boundary: Boundary)

case class Response(body: Body)

import actor.RequestBody.*

object SimulatorActor:
  val dt = 0.001
  val bounds = new Boundary(-4.00, -4.00, 4.00, 4.00)

  def apply(bodies: List[Body],vt: Double = 0.00, i: Int): Behavior[Response] =
    Behaviors setup { ctx =>
      val master = ctx.spawnAnonymous(MasterActor())
      given Timeout = 2.seconds
      given Scheduler = ctx.system.scheduler
      given ExecutionContext = ctx.executionContext
      //Update velocities
      val futureV = Future.sequence(bodies.map(b => master ? (replyTo => Request(UpdateVelocity(b, bodies, dt), replyTo))))
      val updateV = Await.result(futureV, 2.seconds).map(r => r match
        case Response(body) => body)
      //Update positions
      val futureP = Future.sequence(updateV.map(b => master ? (replyTo => Request(UpdatePosition(b, dt), replyTo))))
      val updateP = Await.result(futureP, 2.seconds).map(r => r match
        case Response(body) => body)
      //Check Boundaries
      val futureC = Future.sequence(updateP.map(b => master ? (replyTo => Request(CheckBoundary(b, bounds), replyTo))))
      val updateC = Await.result(futureC, 2.seconds).map(r => r match
        case Response(body) => body)
      SimulatorActor(updateC, vt + dt, i + 1)
    }

object MasterActor:
  def apply(): Behavior[Request] =
    Behaviors receive { (ctx, msg) =>
      val slave = ctx.spawnAnonymous(SlaveActor())
      slave ! msg
      Behaviors.same
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
        body.updateVelocity(acc, dt) ; msg.replyTo ! Response(body) ; Behaviors.stopped
      case UpdatePosition(body: Body, dt: Double) =>
        body.updatePos(dt) ; msg.replyTo ! Response(body) ; Behaviors.stopped
      case CheckBoundary(body: Body, bounds: Boundary) =>
        body.checkAndSolveBoundaryCollision(bounds) ; msg.replyTo ! Response(body) ; Behaviors.stopped
    }