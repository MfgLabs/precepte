package com.mfglabs
package precepte

import shapeless.tag
import shapeless.tag.@@

import scala.concurrent.duration._

/** A measure is an instrumentation that generate a metric for some
  * computation {{{subjectToMeasure}}} and send generated metrics to
  * a collection of consumers.
  *
  * @tparam F a functor (generally a monad)
  * @tparam A the type of the metric generated
  */
trait Measure[F[_], A] { self =>

  /** Compute a metric of type [[A]] from [[subjectToMesure]]. Then
    * send this metric to all consumers. Finally returns a {{{F[B]}}}
    * that must be observationally equivalent to [[subjectToMesure]].
    *
    * The measure SHOULD NEVER halter the behavior: the only effect
    * introduced by the measure should be sending the metrics.
    *
    * @param subjectToMesure
    * @param consumers
    * @tparam B
    * @return
    */
  def measure[B](subjectToMesure: F[B])(consumers: Vector[A => F[Unit]]): F[B]

  @inline final def withConsumers(consumers: Vector[A => F[Unit]]): F ~~> F =
    new (F ~~> F) {
      def apply[B](fb: F[B]): F[B] = self.measure(fb)(consumers)
    }

  @inline final def withConsumer(consumers: A => F[Unit]): F ~~> F =
    withConsumers(Vector(consumers))
}

object Measure {

  /** Represents
    *
    * @param startTime start time of computation execution
    * @param endTime end time of computation execution
    * @param now time when the measure was created
    * @param exception is Some(e) if the computation failed, raising exception e.
    *                  or None if the computation succeeded.
    * @param state the precepte state of the computation
    * @tparam T precepte Tags
    * @tparam M precepte Managed state
    * @tparam U precepte Unmanaged state
    */
  final case class TimeMeasurement[T, M, U](
      startTime: Long @@ NANOSECONDS.type,
      endTime: Long @@ NANOSECONDS.type,
      now: Long @@ MILLISECONDS.type,
      exception: Option[Throwable],
      state: PState[T, M, U]
  )

  final class TimeMeasure[T, M, U, F[_]]
      extends Measure[Precepte[T, M, U, F, ?], TimeMeasurement[T, M, U]] {

    /** Compute a metric of type [[A]] from [[subjectToMesure]]. Then
      * send this metric to all consumers. Finally returns a {{{F[B]}}}
      * that must be observationally equivalent to [[subjectToMesure]].
      *
      * The measure SHOULD NEVER halter the behavior: the only effect
      * introduced by the measure should be sending the metrics.
      *
      * @param subjectToMesure
      * @param consumers
      * @tparam B
      * @return
      */
    def measure[B](subjectToMesure: Precepte[T, M, U, F, B])(
        consumers: Vector[
          TimeMeasurement[T, M, U] => Precepte[T, M, U, F, Unit]])
      : Precepte[T, M, U, F, B] = {

      object P extends Precepte.API[T, M, U, F]

      for {
        state <- P.get
        startTime <- P.delay(tag[NANOSECONDS.type](System.nanoTime()))
        result <- subjectToMesure.attempt
        endTime <- P.delay(tag[NANOSECONDS.type](System.nanoTime()))
        now <- P.delay(tag[MILLISECONDS.type](System.currentTimeMillis()))
        mes = TimeMeasurement(startTime,
                              endTime,
                              now,
                              result.fold(Some(_), _ => None),
                              state)
        _ <- consumers.foldLeft(P.pure(())) {
          case (acc, consumer) =>
            // A consumer failing should not make the whole computation failing too!
            P.map2(acc, consumer(mes).recoverWithValue(()))((_, _) => ())
        }
        r <- Precepte.fromTry(result)
      } yield r
    }
  }
}
