package com.mfglabs
package precepte

import scala.language.higherKinds

import default._

case class Logback(env: BaseEnv) {

  import net.logstash.logback.marker.Markers._
  import org.slf4j.LoggerFactory

  private val logger = LoggerFactory.getLogger("application");

  val sep = "/"

  case class Logger(span: Span, path: Call.Path[BaseTags]) {
    def p(params: Seq[(String, String)]) = {
      import collection.JavaConverters._
      val callees =
        path.map { c =>
          c.tags.callee.value
        }.mkString(sep, sep, "")

      Map(
        env.environment.name -> env.environment.value,
        "span" -> span.value,
        "path" -> path.map(_.id.value).mkString(sep, sep, ""),
        "callees" -> callees,
        "parameters" -> Map(params:_*).asJava).asJava
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