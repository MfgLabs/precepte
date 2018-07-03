resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  Resolver.url(
  "tpolecat-sbt-plugin-releases",
    url("http://dl.bintray.com/content/tpolecat/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns),
  Resolver.jcenterRepo
) 


addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.6.15")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")

addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.4")

addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.4")
