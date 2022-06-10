package controller

import akka.actor.{ActorRef, ActorSystem, Props}
import view.ViewActor
import controller.Simulator

object SimulationMain:
  @main def startSim =
    val system: ActorSystem = ActorSystem.create("GUIsystem")
//    var act: ActorRef = system.actorOf()
    new Simulator(
      2,
      5000,
      Runtime.getRuntime.availableProcessors + 1
    ).execute()
