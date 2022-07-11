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
  var zoneList: List[Zone] = List.empty

  object RootBehavior:
    def apply(zones: List[Zone]): Behavior[Nothing] = Behaviors.setup[Nothing] { ctx =>
      val cluster = Cluster(ctx.system)
      zoneList = zones
      val rainGaugesNumber = ctx.system.settings.config.getInt("rain-analysis.rainGaugesPerNode")
      val zoneNumber = ctx.system.settings.config.getInt("rain-analysis.nodesNumber")
      var gaugeIDs = ctx.system.settings.config.getInt("rain-analysis.rainGaugesNumber")
      cluster.selfMember.roles.head match
        case "rainGauge" =>
          zoneList foreach { z =>
            (0 until rainGaugesNumber) foreach { _ =>
              val newGauge = RainGauge(z.id, Point2D().createRandom(z.bounds.topLeft.x + defPaddingValue, z.bounds.bottomRight.x - defPaddingValue, z.bounds.topLeft.y + defPaddingValue, z.bounds.bottomRight.y - defPaddingValue))
              ctx.spawn(RainGaugeActor(newGauge), s"RainGauge$gaugeIDs")
              gaugeIDs = gaugeIDs - 1
            }
          }
        case "fireStation" => zoneList foreach { z =>
          val newStation = FireStation(z.id, FireStationState.Free, Point2D().createRandom(z.bounds.topLeft.x + defPaddingValue, z.bounds.bottomRight.x - defPaddingValue, z.bounds.topLeft.y + defPaddingValue, z.bounds.bottomRight.y - defPaddingValue))
          ctx.spawn(FireStationActor(newStation), s"FireStation${z.id}")
        }
        case "viewActor" => ctx.spawn(ViewActor(zoneList), "ViewActor")
      Behaviors.empty
    }

  def startup(role: String, port: Int, zoneList: List[Zone]): Unit =
    val config = ConfigFactory.parseString(s"""
           akka.remote.artery.canonical.port=$port
           akka.cluster.roles = [$role]
           """)
      .withFallback(ConfigFactory.load("rain-analysis"))
    ActorSystem[Nothing](RootBehavior(zoneList), "ClusterSystem", config)

  def initZones(): List[Zone] =
    val rows: Int = 2
    val cols: Int = 3
    var id: Int = 0
    val zones = for
      r <- 0 until rows
      c <- 0 until cols
    yield
      id = id + 1
      val bounds = RectangleBounds(Point2D(c * Costants.defalutWidth , r * Costants.defaultHeight))
      Zone(id, ZoneState.Ok, bounds)
    zones.toList

  /*def initRainGauges(zoneID: Int, bounds: RectangleBounds): List[RainGauge] =
    val gauges = for _ <- 0 until 3 yield
      RainGauge(zoneID, Point2D().createRandom(bounds.topLeft.x + defPaddingValue, bounds.bottomRight.x - defPaddingValue, bounds.topLeft.y + defPaddingValue, bounds.bottomRight.y - defPaddingValue))
    gauges.toList*/

  def main(args: Array[String]): Unit =
    val zoneList = initZones()
    if args.isEmpty then
      startup("viewActor", 25251, zoneList)
//      startup("viewActor", 25253, zoneList)
      startup("rainGauge", 4000, zoneList)
      startup("fireStation", 25252, zoneList)
      //      startup("rainGauge", 25251)
      //      startup("rainGauge", 3001)
    else
      require(args.length == 2, "Usage: role port")
      startup(args(0), args(1).toInt, zoneList)
