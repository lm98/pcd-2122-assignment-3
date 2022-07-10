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
  var zones: List[Zone] = List.empty

  object RootBehavior:
    def apply(): Behavior[Nothing] = Behaviors.setup[Nothing] { ctx =>
      val cluster = Cluster(ctx.system)
      zones = initZones()
      val rainGaugesNumber = ctx.system.settings.config.getInt("rain-analysis.rainGaugesPerNode")
      val zoneNumber = ctx.system.settings.config.getInt("rain-analysis.nodesNumber")
      var gaugeIDs = ctx.system.settings.config.getInt("rain-analysis.rainGaugesNumber")
      cluster.selfMember.roles.head match
        case "rainGauge" =>
          (1 until zoneNumber + 1) foreach { x =>
            (0 until rainGaugesNumber) foreach { y =>
              ctx.spawn(RainGaugeActor(x), s"RainGauge$gaugeIDs")
              gaugeIDs = gaugeIDs - 1
            }
          }
        case "fireStation" => (1 until zoneNumber + 1) foreach { x => ctx.spawn(FireStationActor(x), s"FireStation$x")}
        case "viewActor" => ctx.spawn(ViewActor(zones), "ViewActor")
      Behaviors.empty
    }

  def startup(role: String, port: Int): Unit =
    val config = ConfigFactory.parseString(s"""
           akka.remote.artery.canonical.port=$port
           akka.cluster.roles = [$role]
           """)
      .withFallback(ConfigFactory.load("rain-analysis"))
    ActorSystem[Nothing](RootBehavior(), "ClusterSystem", config)

  def initZones(): List[Zone] =
    val rows: Int = 2
    val cols: Int = 3
    var id: Int = 0
    var rainGauges: List[RainGauge] = List()
    val zones = for
      r <- 0 until rows
      c <- 0 until cols
    yield
      id = id + 1
      val bounds = RectangleBounds(Point2D(c * Costants.defalutWidth , r * Costants.defaultHeight))
      rainGauges = initRainGauges(id, bounds)
      Zone(id, ZoneState.Ok, FireStation(id, FireStationState.Free, Point2D().createRandom(bounds.topLeft.x + defPaddingValue, bounds.bottomRight.x - defPaddingValue, bounds.topLeft.y + defPaddingValue, bounds.bottomRight.y - defPaddingValue)), bounds, rainGauges)
    zones.toList

  def initRainGauges(zoneID: Int, bounds: RectangleBounds): List[RainGauge] =
    val gauges = for _ <- 0 until 3 yield
      RainGauge(zoneID, Point2D().createRandom(bounds.topLeft.x + defPaddingValue, bounds.bottomRight.x - defPaddingValue, bounds.topLeft.y + defPaddingValue, bounds.bottomRight.y - defPaddingValue))
    gauges.toList

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      startup("viewActor", 25251)
      startup("rainGauge", 4000)
      startup("fireStation", 25252)
      //      startup("rainGauge", 25251)
      //      startup("rainGauge", 3001)
    else
      require(args.length == 2, "Usage: role port")
      startup(args(0), args(1).toInt)
