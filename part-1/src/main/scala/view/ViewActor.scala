package view

import akka.actor.AbstractActor
import model.{Body, Boundary}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import controller.Commands

object ViewActor:
  enum ViewCommands:
    case Start
    case Stop
    case UpdateView(bodies: List[Body], vt: Double, iteration: Long)

  import ViewCommands.*

  def apply(bounds: Boundary, mainActor: ActorRef[Commands]): Behavior[ViewCommands] =
    Behaviors setup { ctx =>
      val gui = SimulationView(bounds, ctx.self)
      Behaviors receive { (ctx, msg) => msg match
        case Start => mainActor ! Commands.Start ; Behaviors.same
        case Stop => mainActor ! Commands.Stop ; Behaviors.stopped
        case UpdateView(bodies, vt, i) => gui.display(bodies, vt, i) ; Behaviors.same
      }
    }
