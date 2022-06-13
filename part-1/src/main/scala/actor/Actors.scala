package actor

import actor.MasterActor.Commands
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.util.Timeout
import jdk.javadoc.doclet.Reporter
import model.Objects2d.V2d
import model.{Body, Boundary}
import view.ViewActor

import concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future, Promise, blocking}
import scala.util.Success


enum Task:
  case UpdateVelocity(body: Body, others: List[Body], dt: Double)
  case UpdatePosition(body: Body, dt: Double)
  case CheckBoundary(body: Body, boundary: Boundary)

import Task.*

object SimulatorActor:
  sealed trait Commands
  case class Start() extends Commands
  case class Stop() extends Commands
  case class Update(info: Task, bodies: List[Body]) extends Commands

  val dt = 0.001

  def apply(bodies: List[Body], maxIterations: Long, bounds: Boundary, vt: Double = 0.00, i: Int = 0): Behavior[Commands] =
    Behaviors setup { ctx =>
      ctx.log.debug("SimulatorActor: setup")
      val master = ctx.spawnAnonymous(MasterActor(List(), bodies.size, ctx.self))
      Behaviors receive { (ctx, msg) => msg match
        case Start() =>
          ctx.log.debug(s"Starting iteration #$i")
          bodies.foreach(b => master ! MasterActor.Request(UpdateVelocity(b, bodies, dt)))
          Behaviors.same
        case Stop() => Behaviors.stopped
        case Update(UpdateVelocity(_,_,_), updatedBodies) =>
          updatedBodies.foreach(b => master ! MasterActor.Request(UpdatePosition(b, dt)))
          SimulatorActor(updatedBodies, maxIterations, bounds, vt, i)
        case Update(UpdatePosition(_,_), updatedBodies) =>
          updatedBodies.foreach(b => master ! MasterActor.Request(CheckBoundary(b, bounds)))
          SimulatorActor(updatedBodies, maxIterations, bounds, vt, i)
        case Update(CheckBoundary(_,_), updatedBodies) =>
          //viewActor ! ViewActor.ViewCommands.UpdateView(updatedBodies, vt, i)
          ctx.log.debug(s"Iteration #$i: bodies: $updatedBodies")
          SimulatorActor(updatedBodies, maxIterations, bounds, vt + dt, i + 1)
      }
    }

object MasterActor:
  sealed trait Commands
  case class Request(info: Task) extends Commands
  case class Update(info: Task, result: Body) extends Commands

  def apply(bodies: List[Body], nBodies: Int, replyTo: ActorRef[SimulatorActor.Commands]): Behavior[Commands] =
    Behaviors receive { (ctx, msg) => msg match
      case Request(info) =>
        ctx.spawnAnonymous(SlaveActor()) ! SlaveActor.Request(info, ctx.self) ; Behaviors.same
      case Update(info, result) => bodies.size match
        case _ if bodies.size < nBodies => MasterActor(bodies :+ result, nBodies, replyTo)
        case _ if bodies.size == nBodies => replyTo ! SimulatorActor.Update(info, bodies) ; Behaviors.stopped
    }

object SlaveActor:
  import model.Body.*
  case class Request(info: Task, replyTo: ActorRef[MasterActor.Commands])

  def apply(): Behavior[Request] =
    Behaviors receiveMessage { msg => msg.info match
      case UpdateVelocity(body: Body, others: List[Body], dt: Double) =>
        val totalForce = computeTotalForceOnBody(body, others)
        val acc = totalForce :* (1.0/body.mass)
         msg.replyTo ! MasterActor.Update(msg.info, updateVelocity(body, acc, dt)) ; Behaviors.stopped
      case UpdatePosition(body: Body, dt: Double) =>
        msg.replyTo ! MasterActor.Update(msg.info, updatePos(body, dt)) ; Behaviors.stopped
      case CheckBoundary(body: Body, bounds: Boundary) =>
        msg.replyTo ! MasterActor.Update(msg.info, checkAndSolveBoundaryCollision(body, bounds)) ; Behaviors.stopped
    }