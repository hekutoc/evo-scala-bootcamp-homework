import com.typesafe.sbt.packager.docker.{Cmd, ExecCmd}

name := "scala-bootcamp-containers"

version := "0.1"

scalaVersion := "2.13.3"

// From https://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations"
)

val catsEffectVersion = "2.2.0"
val catsVersion = "2.2.0"
val http4sVersion = "0.21.9"
val jakartaMailVersion = "2.0.0"
val logbackVersion = "1.2.3"
val pureconfigVersion = "0.14.0"
val redisClientVersion = "3.30"
val scalatestVersion = "3.2.3"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

lazy val containers = (project in file("."))
  .aggregate(integration)

lazy val integration = (project in file("integration"))
  .configs(IntegrationTest)
  .settings(name := "basic")
  .settings(Defaults.itSettings)
  .settings(libraryDependencies ++= Seq(
    "jakarta.mail" % "jakarta.mail-api" % jakartaMailVersion,
    "com.sun.mail" % "jakarta.mail" % jakartaMailVersion,
    "net.debasishg" %% "redisclient" % redisClientVersion,
    "org.scalatest" %% "scalatest" % scalatestVersion % IntegrationTest
  ))

fork in run := true
