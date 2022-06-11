package controller

import actor.MasterActor
import akka.actor.typed.ActorSystem
import view.ViewActor
import controller.Simulator

object SimulationMain extends App:
  ActorSystem(MasterActor(), "root")
