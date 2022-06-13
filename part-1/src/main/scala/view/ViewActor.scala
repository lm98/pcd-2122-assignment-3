package view

import akka.actor.AbstractActor
import model.{Body, Boundary}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import controller.MainActor

object ViewActor:
  enum ViewCommands:
    case Start
    case Stop
    case UpdateView(bodies: List[Body], vt: Double, iteration: Long)

  import ViewCommands.*

  def apply(bounds: Boundary, mainActor: ActorRef[MainActor.Commands]): Behavior[ViewCommands] =
    Behaviors setup { ctx =>
      ctx.log.debug("ViewActor: Setup")
      val gui = SimulationView(bounds, ctx.self)

      Behaviors receive  { (ctx, msg) => msg match
        case Start => ctx.log.debug("ViewActor: starting MainActor") ; mainActor ! MainActor.Commands.Start ; Behaviors.same
        case Stop => ctx.log.debug("ViewActor: stopping MainActor") ; mainActor ! MainActor.Commands.Stop ; Behaviors.stopped
        case UpdateView(bodies, vt, i) => ctx.log.debug("ViewActor: updating view") ; gui.display(bodies, vt, i) ; Behaviors.same
      }
    }
