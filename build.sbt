lazy val commonSettings =  Seq(
	scalacOptions ++= Seq(
		"-Yrangepos",
		"-Xlint",
		"-deprecation",
		"-Xfatal-warnings",
		"-feature",
		"-encoding", "UTF-8",
		"-unchecked",
		"-Yno-adapted-args",
		"-Ywarn-dead-code",
		"-Ywarn-numeric-widen",
		"-Ywarn-value-discard",
		"-Xfuture",
		"-Ywarn-unused-import"),
	organization := "com.mfglabs",
	scalaVersion := "2.11.4",
	version := "1.0-SNAPSHOT",
	resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
	logLevel in update := Level.Warn
)

lazy val core =
	project.in(file("monitored-core"))
		.settings(commonSettings:_*)
		.settings(
				name := "monitored",
				libraryDependencies ++= Seq(
				  "org.scalaz" %% "scalaz-core" % "7.1.0",
				  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"))

lazy val sample =
	project.in(file("sample"))
		.settings(commonSettings:_*)
		.settings(name := "monitored-sample")
		.dependsOn(core)

lazy val root = project.in(file("."))
	.settings(commonSettings:_*)
	.settings(name := "monitored-root")
	.aggregate(core, sample)