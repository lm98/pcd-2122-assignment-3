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
    val rows: Int = 3
    val cols: Int = 4
    val zones = initZones(rows, cols)
    var port = 25251
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
    startup(port)(ViewActor(zones, rows, cols))
//    startup(port + 1)(ViewActor(zones))
