ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"
lazy val core = (project in file("src/core"))
.settings(
  name := "RiskCore"

)

lazy val client = (project in file("src/client"))
.settings(
  name := "RiskClient",
  libraryDependencies ++= Seq(
    "org.scalafx" %% "scalafx" % "20.0.0-R31",
    "com.typesafe.akka" %% "akka-actor-typed" % "2.8.3",
    "com.typesafe.akka" %% "akka-stream-typed" % "2.8.3",
    "com.typesafe.akka" %% "akka-http" % "10.5.3",
  )
)

lazy val server = (project in file("src/server"))
  .settings(
    name := "RiskServer",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor-typed" % "2.8.3",
      "com.typesafe.akka" %% "akka-stream-typed" % "2.8.3",
      "com.typesafe.akka" %% "akka-http" % "10.5.3",
      "org.scalatest" %% "scalatest" % "3.2.16" % Test
    )
  )


lazy val bot = (project in file("src/bot"))
  .settings(
    name := "RiskBot"
  ).dependsOn(core)

lazy val root = (project in file("."))
  .settings(
    name := "Risk"
  ).aggregate(core, client, server)
