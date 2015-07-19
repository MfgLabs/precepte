package com.mfglabs
package precepte

import scalaz.Monad
import scala.concurrent.{Future, ExecutionContext}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import akka.stream.stage._


object PreStream {

  type P[T, M, U, A] = Precepte[T, M, U, Future, A]

  // can be NOT tail rec
  def toFlow[T, M, U, A](pre: P[T, M, U, A], parallelism: Int = 1)(implicit ex: ExecutionContext, upd: PStateUpdater[T, M, U]): Flow[PState[T, M, U], (PState[T, M, U], A), Unit] = {
    type PS = PState[T, M, U]

    def step[B](p: P[T, M, U, B]): Flow[PS, (PS, B), Unit] =
      p match {
        case Return(a) =>
          Flow[PS].mapAsync(parallelism){ state => Future.successful(state -> a) }

        case Step(st, tags) =>
          Flow[PS].mapAsync(parallelism){ state =>
            val state0 = upd.appendTags(state, tags)
            st.run(state0).map { case (s, p) =>
              Source.single(s).via(step(p))
            }
          }.flatten(FlattenStrategy.concat)
          
        case Flatmap(sub, next) =>
          step(sub()).map { case (state, a) =>
            Source.single(state).via(step(next(a)))
          }.flatten(FlattenStrategy.concat)
      }

    step(pre)
  }
}