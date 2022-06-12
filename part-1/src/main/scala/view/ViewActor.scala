package view

import akka.actor.AbstractActor
import model.{Body, Boundary}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import view.ViewActor.ViewCommands

import java.awt.{BorderLayout, Component, FlowLayout}
import scala.language.{existentials, postfixOps}

trait ViewActor:
  def startSimulation(): Unit
  def stopSimulation(): Unit
  def updateView(): Unit //TODO bodies as parameter??
  def bounds: Boundary

object ViewActor:

  enum ViewCommands:
    case Start
    case Stop
    case Update

  export ViewCommands.*

  def apply(actorRef: ActorRef[ViewActor], bounds: Boundary, w: Int, h: Int): Behavior[ViewCommands] =
    Behaviors.setup( ctx =>
      val viewActor = new ViewActorImpl(ctx.self, bounds)
      val simulationGui = new SimulationView(viewActor, w, h)

      Behaviors.receiveMessage( msg => msg match
        case Start =>
          viewActor.startSimulation()
          Behaviors.same
        case Stop =>
          viewActor.stopSimulation()
          Behaviors.same
        case Update =>
//          simulationGui.display()
          viewActor.updateView()
          Behaviors.same
        )
    )

class ViewActorImpl(actorRef: ActorRef[ViewCommands], override val bounds: Boundary) extends ViewActor:
  override def startSimulation(): Unit = actorRef ! ViewCommands.Start

  override def stopSimulation(): Unit = actorRef ! ViewCommands.Stop

  override def updateView(): Unit = actorRef ! ViewCommands.Update

