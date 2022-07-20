import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.typed.Cluster
import cluster.firestation.FireStationActor
import cluster.raingauge.RainGaugeActor
import cluster.view.ViewActor
import com.typesafe.config.ConfigFactory
import model.{Costants, FireStation, FireStationState, Point2D, RainGauge, RectangleBounds, Zone, ZoneState}

import scala.language.postfixOps
import scala.util.Random


object App:
  val defPaddingValue: Int = 10

  def startup[T](port: Int)(root: => Behavior[T]): Unit =
    val config = ConfigFactory.parseString(s"""
           akka.remote.artery.canonical.port=$port
           """)
      .withFallback(ConfigFactory.load("rain-analysis"))
    ActorSystem[T](root, "ClusterSystem", config)

  def initZones(rows: Int = 2, cols: Int = 3): List[Zone] =
    var id: Int = 0
    val zones = for
      r <- 0 until rows
      c <- 0 until cols
    yield
      id = id + 1
      val bounds = RectangleBounds(Point2D(c * Costants.defalutWidth , r * Costants.defaultHeight))
      Zone(id, ZoneState.Ok, bounds)
    zones.toList

  def main(args: Array[String]): Unit =
    var port = 25251
    var zones = List.empty[Zone]
    var nViews = 1
    var cols: Int = 3
    var rows: Int = 2

    if args.isEmpty then
      zones = initZones()
    else
      rows = args(0).toInt
      cols = args(1).toInt
      zones = initZones(rows, cols)
      nViews = args(2).toInt

    zones foreach { z =>
      (0 until 3) foreach { _ =>
        val newGauge = RainGauge(z.id, Point2D().createRandom(z.bounds.topLeft.x + defPaddingValue, z.bounds.bottomRight.x - defPaddingValue, z.bounds.topLeft.y + defPaddingValue, z.bounds.bottomRight.y - defPaddingValue))
        startup(port)(RainGaugeActor(newGauge))
        port = port + 1
      }
      val newStation = FireStation(z.id, FireStationState.Free, Point2D().createRandom(z.bounds.topLeft.x + defPaddingValue, z.bounds.bottomRight.x - defPaddingValue, z.bounds.topLeft.y + defPaddingValue, z.bounds.bottomRight.y - defPaddingValue))
      startup(port)(FireStationActor(newStation))
      port = port +1
    }
    (0 until nViews) foreach { _ =>
      startup(port)(ViewActor(zones, rows, cols))
      port = port + 1
    }