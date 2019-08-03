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

import cats.Applicative
import cats.free.Trampoline

import scala.concurrent.Future

package object precepte {

  /** Transform each Precepte SubStep
    * @tparam T Precepte Tags
    * @tparam M Precepte Managed State
    * @tparam U Precepte Unmanaged State
    * @tparam F Base effect
    */
  type SubStepInstumentation[T, M, U, F[_]] =
    Precepte[T, M, U, F, ?] ~~> Precepte[T, M, U, F, ?]

  type MetaMonadPrecepteLifter[T, M, U, F[_], A] =
    PState[T, M, U] => Trampoline[F[(PState[T, M, U], A)]]

  def metaMonadPrecepteLifterNaturalTransformation[T, M, U, F[_]](
      implicit F: MetaFunctor[F])
    : F ~~> MetaMonadPrecepteLifter[T, M, U, F, ?] =
    new (F ~~> MetaMonadPrecepteLifter[T, M, U, F, ?]) {
      def apply[A](fa: F[A]): MetaMonadPrecepteLifter[T, M, U, F, A] =
        (s: PState[T, M, U]) =>
          Trampoline.done(F.map(fa) { a: A =>
            (s, a)
          })
    }

  implicit def metaMonadPrecepteLifterMonad[T, M, U, F[_]](
      implicit F: MetaMonad[F],
      upd: PStateUpdater[T, M, U],
      S: MetaSemigroup[U])
    : MetaMonadPrecepte[T, M, U, MetaMonadPrecepteLifter[T, M, U, F, ?]] =
    new (MetaMonadPrecepte[T, M, U, MetaMonadPrecepteLifter[T, M, U, F, ?]]) {
      final type State = PState[T, M, U]
      final type G[X] = Trampoline[F[(State, X)]]

      @inline final def pure[A](x: A): MetaMonadPrecepteLifter[T, M, U, F, A] =
        (s: State) => Trampoline.done(F.pure((s, x)))

      @inline final def get
        : MetaMonadPrecepteLifter[T, M, U, F, PState[T, M, U]] =
        (s: State) => Trampoline.done(F.pure((s, s)))

      @inline final def set(
          s: PState[T, M, U]): MetaMonadPrecepteLifter[T, M, U, F, Unit] =
        (_: State) => Trampoline.done(F.pure((s, ())))

      @inline final def defer[A](ga: => MetaMonadPrecepteLifter[T, M, U, F, A])
        : MetaMonadPrecepteLifter[T, M, U, F, A] =
        (s: State) => Trampoline.defer(ga(s))

      import cats.instances.function._

      @inline final def map[A, B](fa: MetaMonadPrecepteLifter[T, M, U, F, A])(
          f: A => B): MetaMonadPrecepteLifter[T, M, U, F, B] =
        (s: State) => fa(s).map(x => F.map(x) { case (s1, a) => (s1, f(a)) })

      @inline final def flatMap[A, B](
          pla: MetaMonadPrecepteLifter[T, M, U, F, A])(
          f: A => MetaMonadPrecepteLifter[T, M, U, F, B])
        : MetaMonadPrecepteLifter[T, M, U, F, B] =
        (s0: State) =>
          pla(s0).map[F[(State, B)]] { (fa: F[(State, A)]) =>
            F.flatMap[(State, A), (State, B)](fa) {
              case (s1, a) =>
                val tb: G[B] = f(a)(s1)
                tb.run: F[(State, B)]
            }
        }

      @inline final def ap[A, B](pla: MetaMonadPrecepteLifter[T, M, U, F, A])(
          plf: MetaMonadPrecepteLifter[T, M, U, F, A => B])
        : MetaMonadPrecepteLifter[T, M, U, F, B] =
        (s0: State) =>
          Applicative[Trampoline]
            .map2[F[(State, A)], F[(State, A => B)], F[(State, B)]](
              pla(s0),
              plf(s0)
            ) { (fa: F[(State, A)], ff: F[(State, A => B)]) =>
              F.ap(fa)(F.map(ff) {
                case (sff, g) => {
                  case (sfa, a) =>
                    val s = upd.updateUnmanaged(sfa,
                                                S.combine(sfa.unmanaged,
                                                          sff.unmanaged))
                    (s, g(a))
                }
              })
          }
    }

  /** Dummy implicit to declare Future is trampolined */
  implicit val trampolinedFuture: Trampolined[Future] =
    new Trampolined[Future] {
      def defer[A](ga: => Future[A]): Future[A] = ga
    }
}
