package controller

import actor.SimulatorActor
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import model.Objects2d.{P2d, V2d}
import model.{Body, Boundary}
import view.{SimulationView, ViewActor}

import scala.util.Random

object MainActor:
  enum Commands:
    case Start
    case Stop

  import Commands.*

  def createBodies(bounds: Boundary, nBodies: Int): List[Body] =
    //List(Body(1, P2d(0,0), V2d(0,1), 10), Body(2, P2d(1,1), V2d(1,0), 10))
    val rand = new Random(System.currentTimeMillis)
    val bodies = for i <- 0 until nBodies yield
      val x: Double = bounds.x0 * 0.25 + rand.nextDouble * (bounds.x1 - bounds.x0) * 0.25
      val y: Double = bounds.y0 * 0.25 + rand.nextDouble * (bounds.x1 - bounds.y0) * 0.25
      Body(i, P2d(x, y), V2d(x, y), 10.0)
    bodies.toList

  def createBounds(): Boundary = Boundary(-4.00, -4.00, 4.00, 4.00)

  def apply(nBodies: Int, maxIterations: Long): Behavior[Commands] =
    Behaviors setup { ctx =>
      ctx.log.debug("MainActor: Setup")
      val bounds = createBounds()
      val viewActor = ctx.spawn(ViewActor(bounds, ctx.self), "ViewActor")
      val simulator = ctx.spawn(SimulatorActor(createBodies(bounds, nBodies),maxIterations, bounds, viewActor), "SimulationActor")

      Behaviors receive  { (ctx,msg) => msg match
        case Start => ctx.log.debug("MainActor: Starting simulation") ; simulator ! SimulatorActor.Start() ; Behaviors.same
        case Stop => Behaviors.stopped
      }
    }

object SimulationMain extends App:
  main(100, 1000)

  @main def main(nBodies: Int, maxIterations: Long): Unit =
    val main = ActorSystem(MainActor(nBodies, maxIterations), "main")
    main ! MainActor.Commands.Start



