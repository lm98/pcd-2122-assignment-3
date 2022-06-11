package view

import model.Boundary
import akka.actor.typed.ActorRef
import view.ViewActor._
import view.ViewActor.ViewCommands

trait ViewUtils:
  def startSimulation(): Unit
  def stopSimulation(): Unit
  def bounds: Boundary

export ViewUtils.*

object ViewUtils:
  def apply(viewActor: ActorRef[ViewCommands], bounds: Boundary, w: Int, h: Int): ViewUtils = ViewUtilsImpl(viewActor, bounds, w, h)

  class ViewUtilsImpl(viewActor: ActorRef[ViewCommands],val bounds: Boundary, val w: Int, val h: Int) extends ViewUtils:
    override def startSimulation(): Unit = viewActor ! Start
    override def stopSimulation(): Unit = viewActor ! Stop