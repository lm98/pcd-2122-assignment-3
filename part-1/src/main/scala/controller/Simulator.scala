package controller

import conc.controller.Controller
import conc.model.monitor.{StartSync, StartSyncImpl, StopFlag, StopFlagImpl}
import conc.model.{Body, Boundary, P2d, V2d}
import conc.view.SimulationView
import actor.BodyActor

import java.util.{ArrayList, Random}
import scala.+:

class Simulator(val nBodies: Int, val nSteps: Int, val nWorkers: Int):
  val bounds: Boundary = new Boundary(-4.0, -4.0, 4.0, 4.0)
  val rand = new Random(System.currentTimeMillis)
  val bodies: Seq[Body] = for i <- 0 until nBodies yield
      val x: Double = bounds.getX0 * 0.25 + rand.nextDouble * (bounds.getX1 - bounds.getX0) * 0.25
      val y: Double = bounds.getY0 * 0.25 + rand.nextDouble * (bounds.getY1 - bounds.getY0) * 0.25
      new Body(i, new P2d(x, y), new V2d(0, 0), 10)

  def execute(): Unit =
    /** SETUP **/
    val sync: StartSync = new StartSyncImpl
    val flag: StopFlag = new StopFlagImpl
    val controller: Controller = Controller(sync, flag)
    val viewer: SimulationView = SimulationView(620, 620)
    viewer.registerListener(controller)

    var vt = 0.0
    var dt = 0.001
    var iter = 0
//    var bodyActor: BodyActor = ???

@main def testMain: Unit =
  new Simulator(10, 2, 1)