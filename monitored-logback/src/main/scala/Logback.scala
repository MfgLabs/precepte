package com.mfglabs.monitoring

import scala.language.higherKinds
import Monitored.Call._

case class Logback[T](env: Tags.Environment, hostname: Tags.Host) {

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

      (Map(params:_*) ++ Map(
        env.name -> env.value,
        hostname.name -> hostname.value,
        "span" -> span.value,
        "path" -> path.map(_.id.value).mkString(sep, sep, ""),
        "callees" -> callees)).asJava
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