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
  var zones: List[Zone] = List.empty
  object RootBehavior:
    def apply(): Behavior[Nothing] = Behaviors.setup[Nothing] { ctx =>
      val cluster = Cluster(ctx.system)
      zones = initZones()
//      initRainGauges() //todo per finta
      cluster.selfMember.roles.head match
        case "rainGauge" => ctx.spawn(RainGaugeActor(), "RainGauge"+Random.nextInt(10))
        case "fireStation" => ctx.spawn(FireStationActor(), "FireStation")
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
    val zones = for
      r <- 0 until rows
      c <- 0 until cols
    yield
      id = id + 1
      val bounds = RectangleBounds(Point2D(c * Costants.defalutWidth , r * Costants.defaultHeight)) //Point2D(rand.between(c * Costants.defaultHeight, r * Costants.defalutWidth), rand.between((c + 1) * Costants.defaultHeight - 1, (r + 1) * Costants.defalutWidth - 1))
      Zone(id, ZoneState.Ok, FireStation(id, FireStationState.Free, Point2D().createRandom(bounds.topLeft.x, bounds.topLeft.y, bounds.bottomRight.x, bounds.bottomRight.y)), bounds)
    initRainGauges()
    zones.toList

  def initRainGauges(): Unit =
    zones.foreach(zone =>
      val gauges = for _ <- 0 until 3 yield
        RainGauge(zone.id, Point2D().createRandom(zone.bounds.topLeft.x, zone.bounds.topLeft.y, zone.bounds.bottomRight.x, zone.bounds.bottomRight.y))
      zone.rainGauges = gauges.toList
      println(s"zone ${zone.rainGauges}")
    )
  
  def main(args: Array[String]): Unit =
    if args.isEmpty then
      startup("rainGauge", 25251)
      startup("rainGauge", 3000)
      startup("rainGauge", 3001)
      startup("fireStation", 25252)
      startup("viewActor", 3003)
    else
      require(args.length == 2, "Usage: role port")
      startup(args(0), args(1).toInt)
