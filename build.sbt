import play.PlayImport.PlayKeys._

lazy val commonSettings =  Seq(
    organization := "com.mfglabs"
  , version := "0.1.5-SNAPSHOT"
  , scalaVersion := "2.11.7"
  , resolvers ++= Seq(
      "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
    , "Oncue Bintray Repo" at "http://dl.bintray.com/oncue/releases"
    , "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"  //for play 2.3.9
    , "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots" //for play 2.3.9
  )
  , logLevel in update := Level.Warn
)

lazy val strictScalac =
  scalacOptions ++= Seq(
    "-Yrangepos"
    , "-Xlint"
    ,"-deprecation"
    , "-Xfatal-warnings"
    , "-feature"
    , "-encoding", "UTF-8"
    //, "-unchecked"
    , "-Yno-adapted-args"
    , "-Ywarn-dead-code"
    , "-Ywarn-numeric-widen"
    , "-Ywarn-value-discard"
    , "-Xfuture"
    //, "-Ywarn-unused-import"
)

lazy val publishSettings =
   Seq(publishTo := {
      val s3Repo = "s3://mfg-mvn-repo"
      if (isSnapshot.value)
        Some("snapshots" at s3Repo + "/snapshots")
      else
        Some("releases" at s3Repo + "/releases")
    })

lazy val core =
  project.in(file("precepte-core"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(publishSettings:_*)
    .settings(
      name := "precepte-core",
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= Seq(
          "org.scalaz"      %% "scalaz-core"      % "7.1.0"
        , "com.chuusai"     %% "shapeless"        % "2.2.4"
        , "org.scalatest"   %  "scalatest_2.11"   % "2.2.1"   % "test"
      ),
      javaOptions in (Test,run) += "-XX:+UseConcMarkSweepGC -XX:+UseParallelGC -XX:-UseGCOverheadLimit -Xmx8G"
    )

lazy val sample =
  project.in(file("precepte-sample"))
    .enablePlugins(PlayScala)
    .settings(commonSettings:_*)
    .settings(buildInfoSettings: _*)
    .settings(
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := organization.value)
    .settings(
      name := "precepte-sample",
      libraryDependencies ++= Seq(
        jdbc,
        anorm,
        ws,
        "org.specs2" %% "specs2" % "2.4.9"))
    .dependsOn(core, influx, logback, play)

lazy val influx =
  project.in(file("precepte-influx"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(publishSettings:_*)
    .settings(
      name := "precepte-influx",
      libraryDependencies ++= Seq(
        // influx deps
        "com.google.guava" % "guava" % "18.0",
        "com.squareup.retrofit" % "retrofit" % "1.9.0",
        "com.squareup.okhttp" % "okhttp" % "2.4.0"
      ))
    .dependsOn(core)

lazy val logback =
  project.in(file("precepte-logback"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(publishSettings:_*)
    .settings(
      name := "precepte-logback",
      libraryDependencies ++= Seq(
        "ch.qos.logback" % "logback-classic" % "1.1.2",
        "net.logstash.logback" % "logstash-logback-encoder" % "4.2"))
    .dependsOn(core)

lazy val play =
  project.in(file("precepte-play"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(publishSettings:_*)
    .settings(
      libraryDependencies += "com.typesafe.play" %% "play" % "2.3.9",
      name := "precepte-play")
    .dependsOn(core, influx)

lazy val stream =
  project.in(file("precepte-stream"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(publishSettings:_*)
    .settings(
      libraryDependencies ++= Seq(
          "com.typesafe.akka" %% "akka-http-core-experimental" % "1.0"
        , "org.scalatest"     %%  "scalatest"                  % "2.2.1"  % "test"
      ),
      name := "precepte-stream")
    .dependsOn(core)

lazy val root = project.in(file("."))
  .settings(commonSettings:_*)
  .settings(name := "precepte-root")
  .aggregate(core, play, influx, logback, sample, stream)
