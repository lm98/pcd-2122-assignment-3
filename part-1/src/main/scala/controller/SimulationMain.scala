package controller

import akka.actor.{ActorRef, ActorSystem, Props}
import view.ViewActor
import conc.controller.Simulator

object SimulationMain:
  @main def startSim =
    val system: ActorSystem = ActorSystem.create("GUIsystem")
//    var act: ActorRef = system.actorOf()
    new Simulator(
      1000,
      5000,
      Runtime.getRuntime.availableProcessors + 1
    ).execute()
