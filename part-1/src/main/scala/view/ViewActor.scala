package view

import akka.actor.AbstractActor
import model.{Body, Boundary}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import controller.Simulator
import actor.SlaveActor
import view.ViewActor.ViewCommands

import java.awt.{BorderLayout, Component, FlowLayout}
import scala.language.{existentials, postfixOps}

object ViewActor:
  enum ViewCommands:
    case Start
    case Stop
    case Update

  def apply(bounds: Boundary, w: Int, h: Int): Behavior[ViewCommands] =
    Behaviors.setup( ctx =>
      val simulationGui = new SimulationView(w, h, bounds)
      val slaveActor = SlaveActor()
      Behaviors.receiveMessage( msg => msg match
        case ViewCommands.Start =>
          val simulation = new Simulator(100, 100, 2) //todo pass args
          Behaviors.same
        case ViewCommands.Stop =>
          //stop simulation
          Behaviors.same
        case ViewCommands.Update =>
//          simulationGui.display()
          Behaviors.same
        )
    )
