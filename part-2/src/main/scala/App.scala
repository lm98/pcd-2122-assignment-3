import actors.ViewActor
import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.typed.Cluster
import cluster.firestation.FireStation
import cluster.raingauge.RainGauge
import com.typesafe.config.ConfigFactory
import model.{Costants, RectangleBounds, Zone, ZoneState}

import scala.util.Random

object App:
  object RootBehavior:
    def apply(): Behavior[Nothing] = Behaviors.setup[Nothing] { ctx =>
      val cluster = Cluster(ctx.system)
      var zones: List[Zone] = initZones()
      cluster.selfMember.roles.head match
        case "rainGauge" => ctx.spawn(RainGauge(), "RainGauge"+Random.nextInt(10))
        case "fireStation" => ctx.spawn(FireStation(), "FireStation")
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
    val rand = new Random()
    val rows: Int = 2
    val cols: Int = 3
    var x: Int = 0
    val zones = for
      r <- 0 until rows
      c <- 0 until cols
    yield
      x = x + 1
      new Zone(x, ZoneState.Ok, rand.between(1,4), new RectangleBounds(c *  Costants.defalutWidth, r * Costants.defaultHeight))
    zones.toList

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
