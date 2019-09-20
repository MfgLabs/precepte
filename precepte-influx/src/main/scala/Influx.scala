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

import com.mfglabs.precepte.Measure.{TimeMeasure, TimeMeasurement}
import org.influxdb._
import org.influxdb.dto.Point

import scala.util.Try

final class Influx[C: MetaSemigroup](
    influxdbURL: URL,
    user: String,
    password: String,
    dbName: String,
    retentionPolicy: String
) {

  import scala.util.{Try, Failure, Success}
  private lazy val influxDB: Try[InfluxDB] =
    Influx.createClient(influxdbURL, user, password, dbName)

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def monitor(
      errorHandler: Throwable => Precepte[BaseTags, MS, C, Future, Unit])(
      implicit ex: ExecutionContext)
    : SubStepInstrumentation[BaseTags, MS, C, Future] =
    influxDB match {
      case Success(in) =>
        (new TimeMeasure[BaseTags, MS, C, Future]).withConsumer(
          Influx
            .timeMeasurementConsumer(in, dbName, retentionPolicy, errorHandler))
      case Failure(_) => ~~>.id
    }
}

object Influx {

  /** Build an Influx measurement */
  def timeMeasurementToPoint[U](tm: TimeMeasurement[BaseTags, MS, U]): Point = {
    val duration = tm.endTime - tm.startTime
    val sep = "/"
    val path = tm.state.managed.path
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
      .time(tm.now, TimeUnit.MILLISECONDS)
      .tag("host", tm.state.managed.env.host.value)
      .tag("environment", tm.state.managed.env.environment.value)
      .tag("version", tm.state.managed.env.version.value)
      .tag("category", category.value)
      // .tag("callees", callees)
      .tag("method", method.value)
      .field("span", tm.state.managed.span.value)
      .field("path", p)
      .field("execution_time", duration)

    tm.exception.foreach { e =>
      builder.field("error", s"${e.getClass.getName}: ${e.getMessage}")
    }

    builder.build()
  }

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def timeMeasurementConsumer[U](
      client: InfluxDB,
      dbName: String,
      retentionPolicy: String,
      errorHandler: Throwable => Precepte[BaseTags, MS, U, Future, Unit])(
      tm: TimeMeasurement[BaseTags, MS, U])(
      implicit executionContext: ExecutionContext)
    : Precepte[BaseTags, MS, U, Future, Unit] =
    Precepte
      .deferredLift[BaseTags, MS, U, Future, Unit](
        Future(
          client.write(dbName, retentionPolicy, timeMeasurementToPoint(tm))))
      .recoverWith(errorHandler)

  def createClient(influxdbURL: URL,
                   user: String,
                   password: String,
                   dbName: String): Try[InfluxDB] =
    Try {
      val in = InfluxDBFactory.connect(influxdbURL.toString, user, password)
      in.createDatabase(dbName)
      in.enableBatch(2000, 3, TimeUnit.SECONDS)
    }
}
