package actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import model.Zone
import view.ViewImpl

enum ViewActorCommands:
  case FixZone(zoneId: Int)

//todo wrong, must be one zone each actor
class ViewActor(var zones: List[Zone], ctx: ActorContext[_], val zoneId: Int):
  var view: ViewImpl = new ViewImpl(zones)