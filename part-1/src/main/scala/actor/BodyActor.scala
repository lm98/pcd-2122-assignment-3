package actor

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import conc.model.{Body, Boundary, V2d}

object BodyActor:
  enum Task:
    case UpdateVelocity
    case UpdatePosition
    case CheckBoundary

  import Task.*

  def computeTotalForceOnBody(body: Body, bodies: List[Body]): V2d =
    val totalForce: V2d = new V2d(0,0)
    bodies.filter(b => !b.equals(body)).foreach(b => {
      val forceOtherBody = body.computeRepulsiveForceBy(b)
      totalForce.sum(forceOtherBody)
    })
    totalForce.sum(body.getCurrentFrictionForce)

  def apply(bodies: List[Body], body: Body, boundary: Boundary, dt: Double): Behavior[Task] =
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