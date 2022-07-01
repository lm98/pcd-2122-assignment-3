package actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import model.Zone
import view.ViewImpl




object ViewActor:
  enum ViewActorCommands:
    case FixZone(zoneId: Int)
  val viewService: ServiceKey[ViewActorCommands] = ServiceKey[ViewActorCommands]("ViewService")

  def apply(zones: List[Zone], zoneId: Int): Behavior[ViewActorCommands] = //todo wrong, might be one zone each actor
    var view: ViewImpl = new ViewImpl(zones)
    Behaviors setup {ctx =>
      ctx.system.receptionist ! Receptionist.register(viewService, ctx.self)
      running(ctx)
    }

  private def running(ctx: ActorContext[ViewActorCommands]): Behavior[ViewActorCommands] =

    Behaviors receiveMessage { msg => msg match
      case ViewActorCommands.FixZone(zoneId) => ???

    }

