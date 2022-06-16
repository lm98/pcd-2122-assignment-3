package model

import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.util.Timeout
import jdk.javadoc.doclet.Reporter
import model.Objects2d.{P2d, V2d}
import model.{Body, Boundary}
import view.ViewActor

import scala.concurrent.*
import scala.concurrent.duration.DurationInt
import scala.util.Success


enum Task:
  case UpdateVelocity(others: List[Body], dt: Double)
  case UpdatePosition(dt: Double)
  case CheckBoundary(boundary: Boundary)

case class ReceiveTaskResult(info: Task, result: Body)

import model.Task.*

object SimulatorActor:
  sealed trait Commands
  case class Start() extends Commands
  case class Stop() extends Commands
  case class Update(info: Task, bodies: List[Body]) extends Commands

  val dt = 0.001

  def apply(bodies: List[Body], bodyActors: List[ActorRef[BodyActor.Request]], maxIterations: Long, bounds: Boundary, viewActor: ActorRef[ViewActor.ViewCommands],
            started: Boolean = false ,vt: Double = 0.00, i: Int = 0): Behavior[Commands] =
    Behaviors setup { ctx =>
      if started then ctx.self ! SimulatorActor.Start()
      Behaviors receive { (ctx, msg) => msg match
        case Start() =>
          val buffer = ctx.spawnAnonymous(SimulationBufferActor(List(), bodyActors.size, ctx.self))
          bodyActors.foreach(b => b ! BodyActor.Request(UpdateVelocity(bodies, dt), buffer))
          SimulatorActor(bodies, bodyActors,maxIterations, bounds, viewActor, false, vt, i)
        case Stop() => Behaviors.stopped
        case Update(UpdateVelocity(_,_), updatedBodies) =>
          val buffer = ctx.spawnAnonymous(SimulationBufferActor(List(), bodyActors.size, ctx.self))
          bodyActors.foreach(b => b ! BodyActor.Request(UpdatePosition(dt), buffer))
          SimulatorActor(updatedBodies, bodyActors,maxIterations, bounds, viewActor,false, vt, i)
        case Update(UpdatePosition(_), updatedBodies) =>
          val buffer = ctx.spawnAnonymous(SimulationBufferActor(List(), bodyActors.size, ctx.self))
          bodyActors.foreach(b => b ! BodyActor.Request(CheckBoundary(bounds), buffer))
          SimulatorActor(updatedBodies, bodyActors,maxIterations, bounds,viewActor, false, vt, i)
        case Update(CheckBoundary(_), updatedBodies) if i < maxIterations =>
          viewActor ! ViewActor.ViewCommands.UpdateView(updatedBodies, vt, i)
          SimulatorActor(updatedBodies, bodyActors, maxIterations, bounds, viewActor,true, vt + dt, i + 1)
        case Update(_,_) if i == maxIterations =>
          Behaviors.stopped
      }
    }

object SimulationBufferActor:

  def apply(bodies: List[Body], nBodies: Int, replyTo: ActorRef[SimulatorActor.Commands]): Behavior[ReceiveTaskResult] =
    Behaviors receive { (ctx, msg) => msg match
      case ReceiveTaskResult(info, result) =>
        val updatedBodies = bodies :+ result
        updatedBodies.size match
        case _ if updatedBodies.size < nBodies =>
          SimulationBufferActor(updatedBodies, nBodies, replyTo)
        case _ if updatedBodies.size == nBodies =>
          replyTo ! SimulatorActor.Update(info, updatedBodies) ; Behaviors.stopped
    }

object BodyActor:
  import model.BodyOp.*
  case class Request(info: Task, replyTo: ActorRef[ReceiveTaskResult])

  def apply(body: Body): Behavior[Request] =
    Behaviors receive { (_, msg) => msg.info match
      case UpdateVelocity(others: List[Body], dt: Double) =>
        val totalForce = computeTotalForceOnBody(body, others)
        val acc = totalForce :* (1.0/body.mass)
        val updatedBody = updateVelocity(body, acc, dt)
        msg.replyTo ! ReceiveTaskResult(msg.info, updatedBody) ; BodyActor(updatedBody)
      case UpdatePosition(dt: Double) =>
        val updatedBody = updatePos(body, dt)
        msg.replyTo ! ReceiveTaskResult(msg.info, updatedBody) ; BodyActor(updatedBody)
      case CheckBoundary(bounds: Boundary) =>
        val updatedBody = checkAndSolveBoundaryCollision(body, bounds)
        msg.replyTo ! ReceiveTaskResult(msg.info, updatedBody) ; BodyActor(updatedBody)
    }