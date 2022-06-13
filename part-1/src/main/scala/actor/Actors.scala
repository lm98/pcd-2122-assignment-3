package actor

import actor.MasterActor.Commands
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.util.Timeout
import jdk.javadoc.doclet.Reporter
import model.Objects2d.V2d
import model.{Body, Boundary}

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
  case class Update(info: Task, bodies: List[Body]) extends Commands

  val dt = 0.001

  def apply(bodies: List[Body],vt: Double = 0.00, i: Int, maxIterations: Int): Behavior[Commands] =
    Behaviors receive { (ctx, msg) => msg match
      case _ if i == maxIterations => Behaviors.stopped
      case Start() =>
        val master = ctx.spawnAnonymous(MasterActor(List(), bodies.size, ctx.self))
        bodies.foreach(b => master ! MasterActor.Request(UpdateVelocity(b, bodies, dt)))
        Behaviors.same
      case Update(UpdateVelocity(_,_,_), updatedBodies) =>
        val master = ctx.spawnAnonymous(MasterActor(List(), bodies.size, ctx.self))
        updatedBodies.foreach(b => master ! MasterActor.Request(UpdatePosition(b, dt)))
        SimulatorActor(updatedBodies, vt, i, maxIterations)
      case Update(UpdatePosition(_,_), updatedBodies) =>
        val master = ctx.spawnAnonymous(MasterActor(List(), bodies.size, ctx.self))
        val bounds = Boundary(-4.00, -4.00, 4.00, 4.00)
        updatedBodies.foreach(b => master ! MasterActor.Request(CheckBoundary(b, bounds)))
        SimulatorActor(updatedBodies, vt, i, maxIterations)
      case Update(CheckBoundary(_,_), updatedBodies) => SimulatorActor(updatedBodies, vt + dt, i + 1, maxIterations)
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
        case _ if bodies.size == nBodies - 1 => replyTo ! SimulatorActor.Update(info, bodies) ; Behaviors.stopped
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