import play.twirl.sbt.Import.TwirlKeys

lazy val warts = Warts.allBut(
  Wart.ToString, // Needed because of Macro.param
  Wart.Throw, // Needed because of Macros
  Wart.Overloading,
  Wart.DefaultArguments,
  Wart.Recursion,
  Wart.Any,
  Wart.Nothing
)

lazy val catsVersion = "1.6.1"

val safeScalaOptionsCommon =
  Seq(
    "-deprecation",
    "-encoding",
    "UTF8",
    "-explaintypes",
    "-feature",
    "-language:-dynamics",
    "-language:postfixOps",
    "-language:reflectiveCalls",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:experimental.macros",
    "-unchecked",
    "-Xlint:_",
    "-Yno-adapted-args",
    "-Ypartial-unification",
    "-Ywarn-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-inaccessible",
    "-Ywarn-infer-any",
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused-import",
    "-Ywarn-value-discard"
  )

val safeScalaOptionsCommon_2_12 =
  Seq(
    "-Ywarn-extra-implicit",
    "-Ywarn-macros:both",
    "-Ywarn-self-implicit",
    "-Ywarn-unused:_"
  )

val safeScalaOptionsCommon_2_11 =
  Seq(
    "-Ywarn-unused"
  )


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
  , version := "0.4.6-rc3"
  , isSnapshot := false
  , crossScalaVersions := Seq("2.11.12", "2.12.8")
  , resolvers ++= Seq(
      "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
    , "Oncue Bintray Repo" at "http://dl.bintray.com/oncue/releases"
    , "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"  //for play 2.3.9
    , "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots" //for play 2.3.9
  )
  , logLevel in update := Level.Warn  
  , updateOptions := updateOptions.value.withCachedResolution(true)
  , scalacOptions -= "-Xfatal-warnings"
  , scalacOptions ++= safeScalaOptionsCommon ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => safeScalaOptionsCommon_2_11
      case Some((2, 12)) => safeScalaOptionsCommon_2_12
      case Some((p,s)) => throw new Exception(s"Uknown scala binary version $p.$s")
      case _ => throw new Exception(s"Bad scala version: ${scalaVersion.value}")
    }
  }
  , addCompilerPlugin("io.tryp" % "splain" % "0.4.1" cross CrossVersion.patch)
  , addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3" cross CrossVersion.binary)
  , scalafmtOnCompile := true
  , wartremoverErrors in (Compile, compile) := warts
  , wartremoverWarnings in (Compile, console) := warts
) ++ publishSettings :+ (tutTargetDirectory := baseDirectory.value / ".." / "documentation")



lazy val noPublishSettings = Seq(
  publish := (()),
  publishLocal := (()),
  publishArtifact := false
)

lazy val core =
  project
    .in(file("precepte-core"))
    .settings(commonSettings:_*)
    .settings(
      name := "precepte-core",
      libraryDependencies ++= Seq(
        "org.scala-lang"  % "scala-reflect" % scalaVersion.value,
        "com.chuusai"     %% "shapeless"    % "2.3.3",
        "org.typelevel"   %% "cats-free"    % catsVersion,
        "org.scalatest"   %%  "scalatest"   % "3.0.5" % Test
      ),
      javaOptions in (Test,run) += "-XX:+UseConcMarkSweepGC -XX:+UseParallelGC -XX:-UseGCOverheadLimit -Xmx8G"
    )

lazy val coreCats =
  project.in(file("precepte-core-cats"))
    .settings(commonSettings:_*)
    .settings(
      name := "precepte-core-cats",
      libraryDependencies ++= Seq(
          "org.scala-lang"  % "scala-reflect" % scalaVersion.value
        , "org.typelevel"   %% "cats-core"   % catsVersion
        , "org.scalatest"   %% "scalatest"   % "3.0.5" % Test
      )
    )
    .dependsOn(core)


lazy val coreScalaz =
  project.in(file("precepte-core-scalaz"))
    .settings(commonSettings:_*)
    .settings(
      name := "precepte-core-scalaz",
      libraryDependencies ++= Seq(
        "org.scala-lang"  % "scala-reflect" % scalaVersion.value,
          "org.scalaz"      %% "scalaz-core"  % "7.2.28"
        , "org.scalatest"   %%  "scalatest"   % "3.0.5"  % Test
      )
    )
    .dependsOn(core)


lazy val sample =
  project.in(file("precepte-sample"))
    .enablePlugins(PlayScala, BuildInfoPlugin)
    .settings(commonSettings:_*)
    .settings(noPublishSettings:_*)
    .settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := organization.value)
    .settings(
      name := "precepte-sample",
      scalacOptions += "-Yrangepos", // For specs2
      wartremoverExcluded.in(Compile) ++= (routes.in(Compile).value ++ TwirlKeys.compileTemplates.in(Compile).value),
      libraryDependencies ++= Seq(
        jdbc,
        evolutions,
        ws,
        "com.h2database" % "h2" % "1.4.199",
        "com.typesafe.play" %% "anorm" % "2.5.3",
        "org.specs2" %% "specs2-junit" % "4.3.4" % Test,
        "com.typesafe.play" %% "play-specs2" % "2.6.18" % Test
      )
    )
    .dependsOn(coreScalaz, influx, logback, play)

lazy val influx =
  project.in(file("precepte-influx"))
    .settings(commonSettings:_*)
    .settings(
      name := "precepte-influx",
      libraryDependencies += "org.influxdb" % "influxdb-java" % "2.15"
    )
    .dependsOn(core)

lazy val applicationinsights =
  project.in(file("precepte-applicationinsights"))
    .settings(commonSettings:_*)
    .settings(
      name := "precepte-applicationinsights",
      libraryDependencies += "com.microsoft.azure" % "applicationinsights-core" % "2.4.0-BETA"
    )
    .dependsOn(core)

lazy val logback =
  project.in(file("precepte-logback"))
    .settings(commonSettings:_*)
    .settings(
      name := "precepte-logback",
      libraryDependencies ++= Seq(
        "ch.qos.logback" % "logback-classic" % "1.2.3",
        "net.logstash.logback" % "logstash-logback-encoder" % "6.1"))
    .dependsOn(core)

lazy val play =
  project.in(file("precepte-play"))
    .settings(commonSettings:_*)
    .settings(
      libraryDependencies += "com.typesafe.play" %% "play" % "2.6.18" % Provided,
      name := "precepte-play")
    .dependsOn(core)

lazy val stream =
  project.in(file("precepte-stream"))
    .settings(commonSettings:_*)
    .settings(
      libraryDependencies ++= Seq(
          "com.typesafe.akka" %% "akka-stream" % "2.5.16"
        , "org.scalatest"     %%  "scalatest"  % "3.0.8"  % Test
      ),
      name := "precepte-stream")
    .dependsOn(coreScalaz)

lazy val doc =
  project.in(file("precepte-tut"))
    .enablePlugins(TutPlugin)
    .settings(commonSettings:_*)
    .settings(noPublishSettings:_*)
    .dependsOn(core, play, influx, logback, sample, stream)

lazy val root = project.in(file("."))
  .settings(commonSettings:_*)
  .settings(noPublishSettings:_*)
  .settings(name := "precepte-root")
  .aggregate(core, play, influx, applicationinsights, logback, sample, stream, doc, coreScalaz, coreCats)
