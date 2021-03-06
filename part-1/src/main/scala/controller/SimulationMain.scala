package controller

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import model.Objects2d.{P2d, V2d}
import model.{Body, BodyActor, Boundary, SimulatorActor}
import view.{SimulationView, ViewActor}

import scala.util.Random

enum Commands:
  case Start
  case Stop

import Commands.*
import model.SimulationCommands

object MainActor:


  def createBodies(bounds: Boundary, nBodies: Int): List[Body] =
    val rand = new Random(System.currentTimeMillis)
    val bodies = for i <- 0 until nBodies yield
      val x: Double = bounds.x0 * 0.25 + rand.nextDouble * (bounds.x1 - bounds.x0) * 0.25
      val y: Double = bounds.y0 * 0.25 + rand.nextDouble * (bounds.x1 - bounds.y0) * 0.25
      Body(i, P2d(x, y), V2d(x, y), 10.0)
    bodies.toList

  def createBounds(): Boundary = Boundary(-4.00, -4.00, 4.00, 4.00)

  def apply(nBodies: Int, maxIterations: Long): Behavior[Commands] =
    Behaviors setup { ctx =>
      val bounds = createBounds()
      val bodies = createBodies(bounds, nBodies)
      val viewActor = ctx.spawn(ViewActor(bounds, ctx.self), "ViewActor")
      val bodyActors = bodies.map(b => ctx.spawnAnonymous(BodyActor(b)))
      val simulator = ctx.spawn(SimulatorActor(bodies, bodyActors,maxIterations, bounds, viewActor), "SimulationActor")

      Behaviors receive  { (_,msg) => msg match
        case Start => simulator ! SimulationCommands.Start ; Behaviors.same
        case Stop => Behaviors.stopped
      }
    }

object SimulationMain extends App:
  main(1000, 1000)

  @main def main(nBodies: Int, maxIterations: Long): Unit =
    ActorSystem(MainActor(nBodies, maxIterations), "main")