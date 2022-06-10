package view

import akka.actor.AbstractActor
import model.{Body, Boundary}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import java.awt.{BorderLayout, Component, FlowLayout}

object ViewActor:

  enum ViewCommands:
    case Start
    case Stop

  export ViewCommands.*

  def apply(actorRef: ActorRef[_], bounds: Boundary, w: Int, h: Int): Behavior[ViewCommands] =
    Behaviors.setup( ctx => ??? )