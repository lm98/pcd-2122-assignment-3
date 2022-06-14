package actor

import actor.MasterActor.Commands
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.util.Timeout
import jdk.javadoc.doclet.Reporter
import model.Objects2d.{P2d, V2d}
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

  def apply(bodies: List[Body], maxIterations: Long, bounds: Boundary, viewActor: ActorRef[ViewActor.ViewCommands],
            started: Boolean = false ,vt: Double = 0.00, i: Int = 0): Behavior[Commands] =
    Behaviors setup { ctx =>
      ctx.log.debug(s"SimulatorActor: setting up iteration #$i")
      if started then ctx.self ! SimulatorActor.Start()
      Behaviors receive { (ctx, msg) => msg match
        case Start() =>
          ctx.log.debug(s"Starting iteration #$i")
          val master = ctx.spawnAnonymous(MasterActor(List(), bodies.size, ctx.self))
          bodies.foreach(b => master ! MasterActor.Request(UpdateVelocity(b, bodies, dt)))
          SimulatorActor(bodies, maxIterations, bounds, viewActor,true, vt, i)
        case Stop() => Behaviors.stopped
        case Update(UpdateVelocity(_,_,_), updatedBodies) =>
          val master = ctx.spawnAnonymous(MasterActor(List(), bodies.size, ctx.self))
          updatedBodies.foreach(b => master ! MasterActor.Request(UpdatePosition(b, dt)))
          SimulatorActor(updatedBodies, maxIterations, bounds, viewActor,started, vt, i)
        case Update(UpdatePosition(_,_), updatedBodies) =>
          val master = ctx.spawnAnonymous(MasterActor(List(), bodies.size, ctx.self))
          updatedBodies.foreach(b => master ! MasterActor.Request(CheckBoundary(b, bounds)))
          SimulatorActor(updatedBodies, maxIterations, bounds,viewActor, started, vt, i)
        case Update(CheckBoundary(_,_), updatedBodies) if i < maxIterations =>
          viewActor ! ViewActor.ViewCommands.UpdateView(updatedBodies, vt, i)
          ctx.log.debug(s"Iteration #$i: bodies: $updatedBodies")
          SimulatorActor(updatedBodies, maxIterations, bounds, viewActor,started, vt + dt, i + 1)
        case Update(_, updatedBodies) if i == maxIterations =>
          ctx.log.debug(s"Iteration #$i: ending simulation with bodies: $updatedBodies")
          Behaviors.stopped
      }
    }

  @main def testSimulation(): Unit =
    val bodies = List(Body(1, P2d(0,0), V2d(0,0), 10), Body(2, P2d(1,1), V2d(0,0), 10))
    val bounds = Boundary(-4.0, -4.0, 4.0, 4.0)
    val maxIterations = 5
    //val sim = ActorSystem(SimulatorActor(bodies, maxIterations, bounds), "sim")
    //sim ! SimulatorActor.Start()

object MasterActor:
  sealed trait Commands
  case class Request(info: Task) extends Commands
  case class Update(info: Task, result: Body) extends Commands

  def apply(bodies: List[Body], nBodies: Int, replyTo: ActorRef[SimulatorActor.Commands]): Behavior[Commands] =
    Behaviors receive { (ctx, msg) => msg match
      case Request(info) =>
        ctx.log.debug("Sending request to slave")
        ctx.spawnAnonymous(SlaveActor()) ! SlaveActor.Request(info, ctx.self) ; Behaviors.same
      case Update(info, result) =>
        val updatedBodies = bodies :+ result
        updatedBodies.size match
        case _ if updatedBodies.size < nBodies =>
          ctx.log.debug(s"Received $result from slave, ${updatedBodies.size} of $nBodies")
          MasterActor(updatedBodies, nBodies, replyTo)
        case _ if updatedBodies.size == nBodies =>
          ctx.log.debug(s"Received ${updatedBodies.size} results from slave, sending to simulator")
          replyTo ! SimulatorActor.Update(info, updatedBodies) ; Behaviors.stopped
    }

object SlaveActor:
  import model.BodyOp.*
  case class Request(info: Task, replyTo: ActorRef[MasterActor.Commands])

  def apply(): Behavior[Request] =
    Behaviors receive { (ctx, msg) => msg.info match
      case UpdateVelocity(body: Body, others: List[Body], dt: Double) =>
        val totalForce = computeTotalForceOnBody(body, others)
        val acc = totalForce :* (1.0/body.mass)
        ctx.log.debug(s"Update Velocity - Replying to master with result $body")
         msg.replyTo ! MasterActor.Update(msg.info, updateVelocity(body, acc, dt)) ; Behaviors.stopped
      case UpdatePosition(body: Body, dt: Double) =>
        ctx.log.debug(s"Update Position - Replying to master with result $body")
        msg.replyTo ! MasterActor.Update(msg.info, updatePos(body, dt)) ; Behaviors.stopped
      case CheckBoundary(body: Body, bounds: Boundary) =>
        ctx.log.debug(s"CheckBoundary - Replying to master with result $body")
        msg.replyTo ! MasterActor.Update(msg.info, checkAndSolveBoundaryCollision(body, bounds)) ; Behaviors.stopped
    }