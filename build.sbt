scalaVersion := "2.13.3"

val catsVersion = "2.2.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
)


scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
)