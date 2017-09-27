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

import default._

import java.util.concurrent.TimeUnit
import org.influxdb._

case class Influx[C : MetaSemigroup](
  influxdbURL: URL,
  user: String,
  password: String,
  dbName: String,
  retentionPolicy: String
)(implicit ex: ExecutionContext) {

  import scala.util.{ Try, Failure, Success }
  private lazy val influxDB =
    Try {
      val in = InfluxDBFactory.connect(influxdbURL.toString, user, password)
      in.createDatabase(dbName)
      in.enableBatch(2000, 3, TimeUnit.SECONDS)
    }

  type SF[T] = (ST[C], Future[T])

  private def toSerie(startTime: Long, endTime: Long, now: Long, st: ST[C], error: Option[Throwable]) = {
    val duration = endTime - startTime
    val sep = "/"
    val path = st.managed.path
    val p =
      path.map { c =>
        c.id.value
      }.mkString(sep, sep, "")

    val method = path.last.tags.callee.value

    // val callees: String =
    //   path.map(_.tags.callee.value).mkString(sep, sep, "")

    val category =
      path.last.tags.category.value

    val builder = dto.Point.measurement("response_times")
      .time(now, TimeUnit.MILLISECONDS)
      .tag("host", st.managed.env.host.value)
      .tag("environment", st.managed.env.environment.value)
      .tag("version", st.managed.env.version.value)
      .tag("category", category)
      // .tag("callees", callees)
      .tag("method", method)
      .field("span", st.managed.span.value)
      .field("path", p)
      .field("execution_time", duration)

    error.foreach { e =>
      builder.field("error", s"${e.getClass.getName}: ${e.getMessage}")
    }

    builder.build()
  }

  def monitor =
    influxDB match {
      case Success(in) =>
        new (SF ~~> Future) {
          def apply[A](sf: SF[A]): Future[A] = {
            val t0 = System.nanoTime()
            val (st, f) = sf
            f.andThen { case r =>
              val t1 = System.nanoTime()
              val serie = toSerie(t0, t1, System.currentTimeMillis(), st, r.failed.toOption)
              in.write(dbName, retentionPolicy, serie)
            }
          }
        }
      case Failure(e) => throw e
    }


}
