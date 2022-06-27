import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.typed.Cluster
import cluster.firestation.FireStation
import cluster.raingauge.RainGauge
import com.typesafe.config.ConfigFactory

object App:
  object RootBehavior:
    def apply(): Behavior[Nothing] = Behaviors.setup[Nothing] { ctx =>
      val cluster = Cluster(ctx.system)

      cluster.selfMember.roles.head match
        case "rainGauge" => ctx.spawn(RainGauge(), "RainGauge")
        case "fireStation" => ctx.spawn(FireStation(), "FireStation")

      Behaviors.empty
    }

  def startup(role: String, port: Int): Unit =
    val config = ConfigFactory.parseString(s"""
           akka.remote.artery.canonical.port=$port
           akka.cluster.roles = [$role]
           """)
      .withFallback(ConfigFactory.load("rain-analysis"))

    ActorSystem[Nothing](RootBehavior(), "ClusterSystem", config)

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      startup("rainGauge", 25251)
      startup("rainGauge", 0)
      startup("rainGauge", 1)
      startup("fireStation", 25252)
    else
      require(args.length == 2, "Usage: role port")
      startup(args(0), args(1).toInt)
