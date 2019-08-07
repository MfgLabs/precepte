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

import scalaz.Semigroup
import scala.concurrent.{Future, ExecutionContext}
import akka.NotUsed
import akka.stream.FlowShape
import akka.stream.scaladsl._

import corescalaz._

object PreStream {

  type P[T, M, U, A] = Precepte[T, M, U, Future, A]

  /* can be NOT tail rec
     TODO: tailrec!
   */
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def toFlow[T, M, U, A](pre: P[T, M, U, A],
                         parallelism: Int = 1,
                         retries: Int = 3)(
      implicit mo: MetaMonad[Future],
      ex: ExecutionContext,
      upd: PStateUpdater[T, M, U],
      sU: Semigroup[U]
  ): Flow[PState[T, M, U], (PState[T, M, U], A), NotUsed] = {
    type PS = PState[T, M, U]

    def step[B](p: P[T, M, U, B]): Flow[PS, (PS, B), NotUsed] =
      p match {
        case Pure(a) =>
          Flow[PS].mapAsync(parallelism) { state =>
            Future.successful(state -> a)
          }

        case GetPState() =>
          Flow[PS].mapAsync(parallelism) { state =>
            Future.successful(state -> state)
          }

        case RaiseError(e) =>
          Flow[PS].mapAsync(parallelism) { _ =>
            Future.failed(e)
          }

        case CatchError(sub, handler) =>
          Flow[PS].flatMapConcat { state =>
            Source
              .single(state)
              .via(step(sub))
              .recoverWithRetries(
                retries,
                { case e => Source.single(state).via(step(handler(e))) }
              )
          }

        case SetPState(state2) =>
          Flow[PS].mapAsync(parallelism) { _ =>
            Future.successful((state2, ()))
          }

        case Lift(fa) =>
          Flow[PS].mapAsync(parallelism) { state =>
            fa.map(a => state -> a)
          }

        case Defer(deferred) =>
          step(deferred())

        case Mapped(sub, pf) =>
          step(sub).map {
            case (state, a) =>
              state -> pf(a)
          }

        case Flatmap(sub, next) =>
          step(sub).flatMapConcat {
            case (state, a) =>
              Source.single(state).via(step(next(a)))
          }

        case ap @ Apply(pa, pf) =>
          import GraphDSL.Implicits._

          Flow.fromGraph(GraphDSL.create() { implicit b =>
            val bcast = b.add(Broadcast[PS](2))

            val ga: Flow[PS, (PS, ap._A), NotUsed] = step(pa)
            val gf: Flow[PS, (PS, ap._A => B), NotUsed] = step(pf)

            val zip = b.add(ZipWith[(PS, ap._A), (PS, ap._A => B), (PS, B)] {
              case ((ps0, _a), (ps1, f)) =>
                val ps =
                  upd.updateUnmanaged(upd.recombine(ps0, ps0, ps1),
                                      sU.append(ps0.unmanaged, ps1.unmanaged))
                ps -> f(_a)
            })

            bcast.out(0) ~> ga ~> zip.in0
            bcast.out(1) ~> gf ~> zip.in1

            FlowShape(bcast.in, zip.out)
          })

        case ps: SubStep[T, M, U, Future, B] =>
          Flow[PS].mapAsync(parallelism) { state =>
            val state0 = upd.appendTags(state, ps.tags)
            ps.intrumented.run(state0)
          }
      }

    step(pre)
  }
}
