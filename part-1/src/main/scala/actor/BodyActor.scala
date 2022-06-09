package actor

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.ServiceKey
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import conc.model.{Body, Boundary, P2d, V2d}

import scala.concurrent.duration.DurationInt
import scala.collection.mutable.Seq
import scala.concurrent.{ExecutionContext, Future}

object BodyActor:
  enum Task:
    case UpdateVelocity
    case UpdatePosition
    case CheckBoundary

  import Task.*

  def computeTotalForceOnBody(body: Body, bodies: Seq[Body]): V2d =
    val totalForce: V2d = new V2d(0,0)
    bodies.filter(b => !b.equals(body)).foreach(b => {
      val forceOtherBody = body.computeRepulsiveForceBy(b)
      totalForce.sum(forceOtherBody)
    })
    totalForce.sum(body.getCurrentFrictionForce)

  def apply(bodies: Seq[Body], body: Body, boundary: Boundary, dt: Double): Behavior[Task] =
    Behaviors receiveMessage { msg => msg match
        case UpdateVelocity =>
          val totalForce = computeTotalForceOnBody(body, bodies)
          val acc = new V2d(totalForce).scalarMul(1.0/body.getMass)
          body.updateVelocity(acc, dt)
          Behaviors.same
        case UpdatePosition =>
          body.updatePos(dt)
          Behaviors.same
        case CheckBoundary =>
          body.checkAndSolveBoundaryCollision(boundary)
          Behaviors.same
        case _ => Behaviors.stopped
    }

object BodyRender:
  import BodyActor.*
  final case class Render(pos: P2d, vel: V2d, id: ActorRef[_])
  val Service = ServiceKey[Render]("RenderService")
  def apply(): Behavior[Render] = Behaviors.receive { (context, message) =>
    context.log.info("received body with id {}", message.id)
    Behaviors.same
  }

object InteractionPattern extends App:
  import BodyActor.*

  val system = ActorSystem(
    Behaviors.setup[BodyActor.Task] { ctx =>
      val render = ctx.spawn(BodyRender(), "bodyrender")
      given Timeout = 2.seconds
      given Scheduler = ctx.system.scheduler
      given ExecutionContext = ctx.executionContext
      val f: Future[BodyActor.Task] = render ? (task => BodyRender.Render())
    },
    name = "render-body"
  )