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

import scala.concurrent.duration._
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
import shapeless.tag
import shapeless.tag.@@

import scala.collection.JavaConverters._

final class ApplicationInsights[C: MetaSemigroup](instrumentationKey: String,
                                                  useXML: Boolean = true) {

  private lazy val client: Try[TelemetryClient] =
    Try {
      val config =
        if (useXML) TelemetryConfiguration.getActive()
        else TelemetryConfiguration.createDefault()
      config.setInstrumentationKey(instrumentationKey)
      config.setTrackingIsDisabled(false)
      new TelemetryClient(config)
    }

  /** Build a MetricTelemetry instance
    *
    * @param startTime since EPOCh in NANOSECONDS
    * @param endTime since EPOCh in NANOSECONDS
    * @param now since EPOCh in MILLISECONDS
    */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def toTelemetry(startTime: Long @@ NANOSECONDS.type,
                          endTime: Long @@ NANOSECONDS.type,
                          now: Long @@ MILLISECONDS.type,
                          st: ST[C],
                          error: Option[Throwable]): Telemetry = {
    val duration = endTime - startTime
    val sep = "/"
    val path = st.managed.path
    val p =
      path
        .map { c =>
          c.id.value
        }
        .mkString(sep, sep, "")

    val (method, category): (Callee, Category) =
      path.lastOption match {
        case Some(last) => (last.tags.callee, last.tags.category)
        case _          => (Callee("ε"), new Category("ε") {})
      }

    val properties: java.util.Map[String, String] = Map(
      "host" -> st.managed.env.host.value,
      "environment" -> st.managed.env.environment.value,
      "version" -> st.managed.env.version.value,
      "category" -> category.value,
      "method" -> method.value,
      "span" -> st.managed.span.value,
      "path" -> p
    ).asJava

    val executionTimeField = "execution_time"

    val metrics: java.util.Map[String, java.lang.Double] =
      Map[String, java.lang.Double](
        executionTimeField -> duration.toDouble
      ).asJava

    error match {
      case Some(err) =>
        val exceptionTelemetry = new ExceptionTelemetry(err)
        MapUtil.copy(properties, exceptionTelemetry.getContext.getProperties)
        MapUtil.copy(metrics, exceptionTelemetry.getMetrics)
        exceptionTelemetry

      case _ =>
        val metricTelemetry =
          new MetricTelemetry(executionTimeField, duration.toDouble)
        metricTelemetry.setCount(null)
        metricTelemetry.setMin(null)
        metricTelemetry.setMax(null)
        metricTelemetry.setStandardDeviation(null)
        metricTelemetry.setTimestamp(new Date(now))
        MapUtil.copy(properties, metricTelemetry.getProperties)
        metricTelemetry
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def monitor(implicit ec: ExecutionContext)
    : SubStepInstumentation[BaseTags, MS, C, Future] =
    client match {
      case Success(clt) =>
        object P extends PrecepteAPI[BaseTags, MS, C, Future]

        new (P.instrumentStep) {
          def apply[A](p: P.precepte[A]): P.precepte[A] =
            for {
              st <- P.get
              t0 <- P.lift(Future(tag[NANOSECONDS.type](System.nanoTime())))
              r <- p.map(Success(_))
              t1 <- P.lift(Future(tag[NANOSECONDS.type](System.nanoTime())))
              now <- P.lift(
                Future(tag[MILLISECONDS.type](System.currentTimeMillis())))
              telemetry = toTelemetry(t0, t1, now, st, r.failed.toOption)
              _ <- P.lift(Future(clt.track(telemetry)))
              x <- P.lift(Future.fromTry(r))
            } yield x
        }
      case Failure(e) => throw e
    }
}
