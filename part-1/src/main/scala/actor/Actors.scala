package actor

import actor.ManagingActor.Task.ComputeVelocity
import actor.Simulator.Command.Next
import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import conc.model.Body

object ManagingActor:
  enum Task:
    case ComputeVelocity

  def apply(body: Body): Behavior[Task] =
    Behaviors receiveMessage  { msg => msg match
      case ComputeVelocity =>
        Behaviors.same
    }

object Simulator:
  enum Command:
    case Next

  def apply(nBodies: Int, nSteps: Int, currentStep: Int = 0): Behavior[Command] =
    Behaviors setup { ctx =>
      println("Hello, steps: " + currentStep)
      Behaviors receiveMessage  { msg => msg match
        case Next if currentStep < nSteps => Simulator(nBodies, nSteps, currentStep + 1)
        case _ => Behaviors.stopped
      }
    }

  @main def tryActors =
    val system = ActorSystem(Simulator(2,1000), "sim")
    system ! Simulator.Command.Next