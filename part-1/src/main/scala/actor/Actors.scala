package actor

import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.util.Timeout
import jdk.javadoc.doclet.Reporter
import model.Objects2d.V2d
import model.{Body, Boundary}

import concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

case class Request(info: RequestInfo, replyTo: ActorRef[Response])

enum RequestInfo:
  case UpdateVelocity(body: Body, others: List[Body], dt: Double)
  case UpdatePosition(body: Body, dt: Double)
  case CheckBoundary(body: Body, boundary: Boundary)

case class Response(body: Body)

import actor.RequestInfo.*

object SimulatorActor:
  val dt = 0.001
  val bounds = Boundary(-4.00, -4.00, 4.00, 4.00)

  def apply(bodies: List[Body],vt: Double = 0.00, i: Int): Behavior[Response] =
    Behaviors setup { ctx =>
      val master = ctx.spawnAnonymous(MasterActor())
      bodies.foreach(b => master ! Request(UpdateVelocity(b, bodies, dt), ctx.self))
      SimulatorActor(bodies, vt + dt, i + 1)
    }

object MasterActor:
  def apply(): Behavior[Request] =
    Behaviors receive { (ctx, msg) =>
      val slave = ctx.spawnAnonymous(SlaveActor())
      slave ! msg
      Behaviors.same
    }

object SlaveActor:
  import model.Body.*

  def apply(): Behavior[Request] =
    Behaviors receiveMessage { msg => msg.info match
      case UpdateVelocity(body: Body, others: List[Body], dt: Double) =>
        val totalForce = computeTotalForceOnBody(body, others)
        val acc = totalForce :* (1.0/body.mass)
         msg.replyTo ! Response(updateVelocity(body, acc, dt)) ; Behaviors.stopped
      case UpdatePosition(body: Body, dt: Double) =>
        msg.replyTo ! Response(updatePos(body, dt)) ; Behaviors.stopped
      case CheckBoundary(body: Body, bounds: Boundary) =>
        msg.replyTo ! Response(checkAndSolveBoundaryCollision(body, bounds)) ; Behaviors.stopped
    }