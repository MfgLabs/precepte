name := """monitored"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.4"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.specs2" %% "specs2" % "2.4.9",
  "com.chuusai" %% "shapeless" % "2.1.0"
)
