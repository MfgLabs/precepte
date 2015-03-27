package com.mfglabs.monitoring

import scala.language.higherKinds
import Call._

case class Logback(env: Tags.Environment) {

  import net.logstash.logback.marker.Markers._
  import org.slf4j.LoggerFactory

  private val logger = LoggerFactory.getLogger("application");

  val sep = "/"

  case class Logger(span: Span, path: Path) {
    def p(params: Seq[(String, String)]) = {
      import collection.JavaConverters._
      val callees =
        path.flatMap { c =>
          c.tags.values.collect { case Tags.Callee(n) => n }
        }.mkString(sep, sep, "")

      val tags = path.last.tags.values.map(t => t.name -> t.value).toMap.asJava

      Map(
        env.name -> env.value,
        "span" -> span.value,
        "path" -> path.map(_.id.value).mkString(sep, sep, ""),
        "callees" -> callees,
        "parameters" -> Map(params:_*).asJava,
        "tags" -> tags).asJava
    }

    def debug(message: => String, params: (String, String)*): Unit =
      logger.debug(appendEntries(p(params)), message)
    def info(message: => String, params: (String, String)*): Unit =
      logger.info(appendEntries(p(params)), message)
    def warn(message: => String, params: (String, String)*): Unit =
      logger.warn(appendEntries(p(params)), message)
    def error(message: => String, params: (String, String)*): Unit =
      logger.error(appendEntries(p(params)), message)
  }

}