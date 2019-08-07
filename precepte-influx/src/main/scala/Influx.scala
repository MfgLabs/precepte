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

import java.net.URL

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import default._
import java.util.concurrent.TimeUnit

import org.influxdb._
import org.influxdb.dto.Point
import shapeless.tag
import shapeless.tag.@@

final class Influx[C: MetaSemigroup](
    influxdbURL: URL,
    user: String,
    password: String,
    dbName: String,
    retentionPolicy: String
) {

  import scala.util.{Try, Failure, Success}
  private lazy val influxDB: Try[InfluxDB] =
    Try {
      val in = InfluxDBFactory.connect(influxdbURL.toString, user, password)
      in.createDatabase(dbName)
      in.enableBatch(2000, 3, TimeUnit.SECONDS)
    }

  private def toSerie(startTime: Long @@ NANOSECONDS.type,
                      endTime: Long @@ NANOSECONDS.type,
                      now: Long @@ MILLISECONDS.type,
                      st: ST[C],
                      error: Option[Throwable]): Point = {
    val duration = endTime - startTime
    val sep = "/"
    val path = st.managed.path
    val p =
      path
        .map { c =>
          c.id.name
        }
        .mkString(sep, sep, "")

    val (method, category): (Callee, Category) =
      path.lastOption match {
        case Some(last) => (last.tags.callee, last.tags.category)
        case _          => (Callee("ε"), new Category("ε") {})
      }

    val builder = dto.Point
      .measurement("response_times")
      .time(now, TimeUnit.MILLISECONDS)
      .tag("host", st.managed.env.host.value)
      .tag("environment", st.managed.env.environment.value)
      .tag("version", st.managed.env.version.value)
      .tag("category", category.value)
      // .tag("callees", callees)
      .tag("method", method.value)
      .field("span", st.managed.span.value)
      .field("path", p)
      .field("execution_time", duration)

    error.foreach { e =>
      builder.field("error", s"${e.getClass.getName}: ${e.getMessage}")
    }

    builder.build()
  }

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def monitor(implicit ex: ExecutionContext)
    : SubStepInstrumentation[BaseTags, MS, C, Future] =
    influxDB match {
      case Success(in) =>
        object P extends Precepte.API[BaseTags, MS, C, Future]

        new (P.instrumentStep) {
          def apply[A](p: P.precepte[A]): P.precepte[A] =
            for {
              st <- P.get
              t0 <- P.lift(Future(tag[NANOSECONDS.type](System.nanoTime())))
              r <- p.attempt
              _ <- P.lift(Future {
                val t1 = tag[NANOSECONDS.type](System.nanoTime())
                val now = tag[MILLISECONDS.type](System.currentTimeMillis())
                val serie = toSerie(t0, t1, now, st, r.failed.toOption)
                in.write(dbName, retentionPolicy, serie)
              }.recover { case _ => () })
              x <- P.fromTry(r)
            } yield x
        }
      case Failure(e) => throw e
    }
}
