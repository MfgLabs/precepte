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
import scala.language.higherKinds
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.language.postfixOps

import scalaz.~>

import default._

import java.util.concurrent.TimeUnit
import org.influxdb._

object Influx {
  type SF[T] = (ST[Int], Future[T])
}

import Influx.SF

case class Influx[C : scalaz.Semigroup](
  influxdbURL: URL,
  user: String,
  password: String,
  dbName: String
)(implicit ex: ExecutionContext) extends (SF ~> Future){
  private val influxDB = {
    val in = InfluxDBFactory.connect(influxdbURL.toString, user, password)
    in.createDatabase(dbName)
    in.enableBatch(2000, 3, TimeUnit.SECONDS)
  }

  def apply[A](sf: SF[A]): Future[A] = {
    val t0 = System.nanoTime()
    val (st, f) = sf
    f.map { r =>
      val t1 = System.nanoTime()
      val duration = t1 - t0

      val sep = "/"
      val path = st.managed.path
      val p =
        path.map { c =>
          c.id.value
        }.mkString(sep, sep, "")

      val method = path.last.tags.callee.value

      val callees: String =
        path.map(_.tags.callee.value).mkString(sep, sep, "")

      val category =
        path.last.tags.category.value

      val serie =
        dto.Point.measurement("response_times")
          .time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
          .tag("host", st.managed.env.host.value)
          .tag("environment", st.managed.env.environment.value)
          .tag("version", st.managed.env.version.value)
          .tag("category", category)
          .tag("callees", callees)
          .tag("method", method)
          .field("span", st.managed.span.value)
          .field("path", p)
          .field("execution_time", duration)
          .build();

      influxDB.write(dbName, "default", serie)
      r
    }
  }
}