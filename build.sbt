import play.PlayImport.PlayKeys._

lazy val commonSettings =  Seq(
    organization := "com.mfglabs"
  , version := "0.1.4-SNAPSHOT"
  , scalaVersion := "2.11.7"
  , resolvers ++= Seq(
      "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
    , "Oncue Bintray Repo" at "http://dl.bintray.com/oncue/releases"
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

lazy val core =
  project.in(file("precepte-core"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(
      name := "precepte-core",
      publishTo := {
        val s3Repo = "s3://mfg-mvn-repo"
        if (isSnapshot.value)
          Some("snapshots" at s3Repo + "/snapshots")
        else
          Some("releases" at s3Repo + "/releases")
      },
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= Seq(
          "com.chuusai"     %% "shapeless"        % "2.2.4"
        , "org.scalatest"   %  "scalatest_2.11"   % "2.2.1"   % "test"
      ),
      javaOptions in (Test,run) += "-XX:+UseConcMarkSweepGC -XX:+UseParallelGC -XX:-UseGCOverheadLimit -Xmx8G"
    )

lazy val coreScalaz =
  project.in(file("precepte-core-scalaz"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(
      name := "precepte-core-scalaz",
      publishTo := {
        val s3Repo = "s3://mfg-mvn-repo"
        if (isSnapshot.value)
          Some("snapshots" at s3Repo + "/snapshots")
        else
          Some("releases" at s3Repo + "/releases")
      },
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= Seq(
          "org.scalaz"      %% "scalaz-core"      % "7.1.0"
        , "com.chuusai"     %% "shapeless"        % "2.2.4"
        , "org.scalatest"   %  "scalatest_2.11"   % "2.2.1"   % "test"
      ),
      javaOptions in (Test,run) += "-XX:+UseConcMarkSweepGC -XX:+UseParallelGC -XX:-UseGCOverheadLimit -Xmx8G"
    )
    .dependsOn(core)

lazy val coreCats =
  project.in(file("precepte-core-cats"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(
      name := "precepte-core-cats",
      publishTo := {
        val s3Repo = "s3://mfg-mvn-repo"
        if (isSnapshot.value)
          Some("snapshots" at s3Repo + "/snapshots")
        else
          Some("releases" at s3Repo + "/releases")
      },
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= Seq(
          "org.spire-math"  %% "cats"             % "0.2.0"
        , "com.chuusai"     %% "shapeless"        % "2.2.4"
        , "org.scalatest"   %  "scalatest_2.11"   % "2.2.1"   % "test"
      )
    )
    .dependsOn(core)


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
    .settings(
      name := "precepte-influx",
      publishTo := {
        val s3Repo = "s3://mfg-mvn-repo"
        if (isSnapshot.value)
          Some("snapshots" at s3Repo + "/snapshots")
        else
          Some("releases" at s3Repo + "/releases")
      },
      libraryDependencies ++= Seq(ws))
    .dependsOn(core)

lazy val logback =
  project.in(file("precepte-logback"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(
      name := "precepte-logback",
      publishTo := {
        val s3Repo = "s3://mfg-mvn-repo"
        if (isSnapshot.value)
          Some("snapshots" at s3Repo + "/snapshots")
        else
          Some("releases" at s3Repo + "/releases")
      },
      libraryDependencies ++= Seq(
        "ch.qos.logback" % "logback-classic" % "1.1.2",
        "net.logstash.logback" % "logstash-logback-encoder" % "4.2"))
    .dependsOn(core)

lazy val play =
  project.in(file("precepte-play"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(
      publishTo := {
        val s3Repo = "s3://mfg-mvn-repo"
        if (isSnapshot.value)
          Some("snapshots" at s3Repo + "/snapshots")
        else
          Some("releases" at s3Repo + "/releases")
      },
      libraryDependencies += "com.typesafe.play" %% "play" % "2.3.9",
      name := "precepte-play")
    .dependsOn(core, influx)

lazy val stream =
  project.in(file("precepte-stream"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(
      publishTo := {
        val s3Repo = "s3://mfg-mvn-repo"
        if (isSnapshot.value)
          Some("snapshots" at s3Repo + "/snapshots")
        else
          Some("releases" at s3Repo + "/releases")
      },
      libraryDependencies ++= Seq(
          "com.typesafe.akka" %% "akka-http-core-experimental" % "1.0"
        , "org.scalatest"     %%  "scalatest"                  % "2.2.1"  % "test"
      ),
      name := "precepte-stream")
    .dependsOn(core)


lazy val root = project.in(file("."))
  .settings(commonSettings:_*)
  .settings(name := "precepte-root")
  .aggregate(core, coreScalaz, coreCats, play, influx, logback, sample, stream)
