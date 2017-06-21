/*
Copyright 2015 Mfg labs.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package com.mfglabs
package precepte

import default._
import scala.collection.immutable.{ Map => ScMap }

case class Logback(env: BaseEnv, loggerName: String) {

  import net.logstash.logback.marker.Markers._
  import org.slf4j.LoggerFactory

  val logger = LoggerFactory.getLogger(loggerName)

  val sep = "/"

  case class Logger(span: Span, path: Call.Path[BaseTags]) {
    def p(params: Seq[(String, String)]) = {
      import collection.JavaConverters._
      val callees =
        path.map { c =>
          c.tags.callee.value
        }.mkString(sep, sep, "")

      ScMap(
        env.environment.name -> env.environment.value,
        "span" -> span.value,
        "path" -> path.map(_.id.value).mkString(sep, sep, ""),
        "callees" -> callees,
        "parameters" -> ScMap(params:_*).asJava).asJava
    }

    def debug(message: => String, params: Seq[(String, String)] = Nil): Unit =
      logger.debug(appendEntries(p(params)), message)
    def info(message: => String, params: Seq[(String, String)] = Nil): Unit =
      logger.info(appendEntries(p(params)), message)
    def warn(message: => String, params: Seq[(String, String)] = Nil): Unit =
      logger.warn(appendEntries(p(params)), message)
    def error(message: => String, params: Seq[(String, String)] = Nil): Unit =
      logger.error(appendEntries(p(params)), message)
    def error(message: => String, ex: Throwable, params: Seq[(String, String)]): Unit =
      logger.error(appendEntries(p(params)), message, ex)
    def error(message: => String, ex: Throwable): Unit =
      error(message, ex, Nil)
  }

}