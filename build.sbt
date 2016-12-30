import play.PlayImport.PlayKeys._

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/MfgLabs/precepte")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")),
  publishMavenStyle := true,
  publishArtifact in packageDoc := false,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false }
)

lazy val commonSettings =  Seq(
    organization := "com.mfglabs"
  , version := "0.4.3"
  , scalaVersion := "2.11.8"
  , resolvers ++= Seq(
      "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
    , "Oncue Bintray Repo" at "http://dl.bintray.com/oncue/releases"
    , "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"  //for play 2.3.9
    , "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots" //for play 2.3.9
  )
  , logLevel in update := Level.Warn
  , addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
) ++ tutSettings ++ publishSettings :+ (tutTargetDirectory := baseDirectory.value / ".." / "documentation")

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
    // , "-Ylog-classpath"
    //, "-Ywarn-unused-import"
)


lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val core =
  project.in(file("precepte-core"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(
      name := "precepte-core",
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= Seq(
          "com.chuusai"     %% "shapeless"        % "2.2.4"
        , "org.scalatest"   %  "scalatest_2.11"   % "2.2.1"   % "test"
      ),
      javaOptions in (Test,run) += "-XX:+UseConcMarkSweepGC -XX:+UseParallelGC -XX:-UseGCOverheadLimit -Xmx8G"
    )

lazy val coreCats =
  project.in(file("precepte-core-cats"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(
      name := "precepte-core-cats",
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= Seq(
          "org.spire-math"  %% "cats"             % "0.3.0"
        , "org.scalatest"   %  "scalatest_2.11"   % "2.2.1"   % "test"
      )
    )
    .dependsOn(core)


lazy val coreScalaz =
  project.in(file("precepte-core-scalaz"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(
      name := "precepte-core-scalaz",
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= Seq(
          "org.scalaz"      %% "scalaz-core"      % "7.1.0"
        , "org.scalatest"   %  "scalatest_2.11"   % "2.2.1"   % "test"
      )
    )
    .dependsOn(core)


lazy val sample =
  project.in(file("precepte-sample"))
    .enablePlugins(PlayScala)
    .settings(commonSettings:_*)
    .settings(buildInfoSettings: _*)
    .settings(noPublishSettings:_*)
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
    .dependsOn(coreScalaz, influx, logback, play)

lazy val influx =
  project.in(file("precepte-influx"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
    .settings(
      name := "precepte-influx",
      libraryDependencies ++= Seq(
        "org.influxdb" % "influxdb-java" % "2.5"
      ))
    .dependsOn(core)

lazy val logback =
  project.in(file("precepte-logback"))
    .settings(commonSettings:_*)
    .settings(strictScalac)
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
    .settings(
      libraryDependencies += "com.typesafe.play" %% "play" % "2.3.9",
      name := "precepte-play")
    .dependsOn(core)

lazy val doc =
  project.in(file("precepte-tut"))
    .settings(commonSettings:_*)
    .settings(noPublishSettings:_*)
    .settings(strictScalac)
    .dependsOn(core, play, influx, logback, sample)

lazy val root = project.in(file("."))
  .settings(commonSettings:_*)
  .settings(noPublishSettings:_*)
  .settings(name := "precepte-root")
  .aggregate(core, play, influx, logback, sample, doc, coreScalaz, coreCats)
