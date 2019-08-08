resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  Resolver.url(
  "tpolecat-sbt-plugin-releases",
    url("http://dl.bintray.com/content/tpolecat/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns),
  Resolver.jcenterRepo
) 


addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.6.23")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")

addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.12")

addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.4")

addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.5.1")

addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.4.2")

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.7")

addSbtPlugin("org.duhemm" % "sbt-errors-summary" % "0.6.0")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.4")

addSbtPlugin("org.duhemm" % "sbt-errors-summary" % "0.6.0")
