import play.PlayImport.PlayKeys._

lazy val commonSettings =  Seq(
	organization := "com.mfglabs",
	version := "1.0-SNAPSHOT",
	scalaVersion := "2.11.4",
	resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
	logLevel in update := Level.Warn
)

lazy val core =
	project.in(file("monitored-core"))
		.settings(commonSettings:_*)
		.settings(
			publishTo := {
		  val s3Repo = "s3://mfg-mvn-repo"
		  if (isSnapshot.value)
		    Some("snapshots" at s3Repo + "/snapshots")
		  else
		    Some("releases" at s3Repo + "/releases")
		})
		.settings(
			name := "monitored-core",
			libraryDependencies ++= Seq(
			  "org.scalaz" %% "scalaz-core" % "7.1.0",
			  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"),
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
				"-Ywarn-unused-import"))

lazy val sample =
	project.in(file("monitored-sample"))
		.enablePlugins(PlayScala)
		.settings(commonSettings:_*)
		.settings(
			name := "monitored-sample",
			libraryDependencies ++= Seq(
				jdbc,
				anorm,
				"org.specs2" %% "specs2" % "2.4.9"))
		.dependsOn(core)

lazy val root = project.in(file("."))
	.settings(commonSettings:_*)
	.settings(name := "monitored-root")
	.aggregate(core, sample)