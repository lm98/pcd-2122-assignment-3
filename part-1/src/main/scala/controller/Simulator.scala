package controller

import akka.actor.typed.ActorRef
import conc.controller.Controller
import conc.model.monitor.{StartSync, StartSyncImpl, StopFlag, StopFlagImpl}
import conc.model.{Body, Boundary, P2d, V2d}
import view.{SimulationView}
import view.ViewActor.ViewCommands
import java.util
import java.util.{ArrayList, Random}
import scala.collection.mutable
import scala.collection.mutable.Seq
import scala.concurrent.Future

class Simulator(val nBodies: Int, val nSteps: Int, val nWorkers: Int):
  val bounds: Boundary = new Boundary(-4.0, -4.0, 4.0, 4.0)
  val rand = new Random(System.currentTimeMillis)
  val bodies= for i <- 0 until nBodies yield
      val x: Double = bounds.getX0 * 0.25 + rand.nextDouble * (bounds.getX1 - bounds.getX0) * 0.25
      val y: Double = bounds.getY0 * 0.25 + rand.nextDouble * (bounds.getY1 - bounds.getY0) * 0.25
      new Body(i, new P2d(x, y), new V2d(0, 0), 10)
  val list = bodies.toList
  val height = 620
  val width = 620

  def execute(): Unit =
    /** SETUP **/
   /* val sync: StartSync = new StartSyncImpl
    val flag: StopFlag = new StopFlagImpl
    val controller: Controller = Controller(sync, flag)
    val viewer: SimulationView = SimulationView(620, 620)
    viewer.registerListener(controller)
    var results = new Seq[Future[Body]]*/

    var vt = 0.0
    var dt = 0.001
//    var viewer = new SimulationView(utils, height, width)
//    var iter = 0
//    var bodyActor = BodyActor(bodies, b)
    
    /*for iter <- 0 until nSteps do 
      bodies.foreach(b => 
        var bodyActor = BodyActor(bodies, b, bounds, dt)
      )*/
    

@main def testMain: Unit =
  new Simulator(10, 2, 1)