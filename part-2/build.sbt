ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

val AkkaVersion = "2.6.19"
lazy val root = (project in file("."))
  .settings(
    name := "part-2",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion, // For standard log configuration
      "com.typesafe.akka" %% "akka-remote" % AkkaVersion, // For akka remote
      "com.typesafe.akka" %% "akka-cluster-typed" % AkkaVersion, // akka clustering module
      "com.typesafe.akka" %% "akka-serialization-jackson" % AkkaVersion,
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
    )
  )