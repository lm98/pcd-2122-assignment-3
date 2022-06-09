package view

import akka.actor.AbstractActor


object ViewActor extends AbstractActor:
  override def createReceive(): AbstractActor.Receive =
    receiveBuilder().matchAny(msg =>
      println(msg)
    ).build()
