/*
Copyright 2019 Mfg labs.

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

import java.util.Date

import com.mfglabs.precepte.Measure.{TimeMeasure, TimeMeasurement}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import com.microsoft.applicationinsights._
import default._
import com.microsoft.applicationinsights.internal.util.MapUtil
import com.microsoft.applicationinsights.telemetry.{
  ExceptionTelemetry,
  MetricTelemetry,
  Telemetry
}
import scala.collection.JavaConverters._

final class ApplicationInsights[C: MetaSemigroup](instrumentationKey: String,
                                                  useXML: Boolean = true) {

  private lazy val client: Try[TelemetryClient] =
    ApplicationInsights.createClient(instrumentationKey, useXML)

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def monitor(
      errorHandler: Throwable => Precepte[BaseTags, MS, C, Future, Unit])(
      implicit ec: ExecutionContext)
    : SubStepInstrumentation[BaseTags, MS, C, Future] =
    client match {
      case Success(clt) =>
        (new TimeMeasure[BaseTags, MS, C, Future])
          .withConsumer(
            ApplicationInsights.timeMeasurementConsumer(clt, errorHandler))
      case Failure(_) => ~~>.id
    }
}

object ApplicationInsights {

  /** Build a MetricTelemetry instance */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def timeMesurementToTelemetry[U](
      tm: TimeMeasurement[BaseTags, MS, U]): Telemetry = {
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

    val properties: java.util.Map[String, String] = Map(
      "host" -> tm.state.managed.env.host.value,
      "environment" -> tm.state.managed.env.environment.value,
      "version" -> tm.state.managed.env.version.value,
      "category" -> category.value,
      "method" -> method.value,
      "span" -> tm.state.managed.span.value,
      "path" -> p
    ).asJava

    val metrics: java.util.Map[String, java.lang.Double] =
      Map[String, java.lang.Double](
        "execution_time" -> duration.toDouble
      ).asJava

    tm.exception match {
      case Some(err) =>
        val exceptionTelemetry = new ExceptionTelemetry(err)
        MapUtil.copy(properties, exceptionTelemetry.getContext.getProperties)
        MapUtil.copy(metrics, exceptionTelemetry.getMetrics)
        exceptionTelemetry

      case _ =>
        val metricTelemetry =
          new MetricTelemetry(method.value, duration.toDouble)
        metricTelemetry.setCount(null)
        metricTelemetry.setMin(null)
        metricTelemetry.setMax(null)
        metricTelemetry.setStandardDeviation(null)
        metricTelemetry.setTimestamp(new Date(tm.now))
        MapUtil.copy(properties, metricTelemetry.getProperties)
        metricTelemetry
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def timeMeasurementConsumer[U](
      client: TelemetryClient,
      errorHandler: Throwable => Precepte[BaseTags, MS, U, Future, Unit])(
      tm: TimeMeasurement[BaseTags, MS, U])(
      implicit executionContext: ExecutionContext)
    : Precepte[BaseTags, MS, U, Future, Unit] =
    Precepte
      .deferredLift(Future(client.track(timeMesurementToTelemetry(tm))))
      .recoverWith(errorHandler)

  def createClient(instrumentationKey: String,
                   useXML: Boolean): Try[TelemetryClient] =
    Try {
      val config =
        if (useXML) TelemetryConfiguration.getActive()
        else TelemetryConfiguration.createDefault()
      config.setInstrumentationKey(instrumentationKey)
      config.setTrackingIsDisabled(false)
      new TelemetryClient(config)
    }
}
