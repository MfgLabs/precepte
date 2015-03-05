name := """monitored"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.4"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")