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

/** Monad Transformers to add Precepte effects to some monads */
object PrecepteTransformers {

  /** Add a State effect to F[_] with the very specific
    * way Precepte handles its state.
    *
    * @tparam T Type of Precepte Tag
    * @tparam M Type of Precepte Managed State
    * @tparam U Type of Precepte Unmanaged State
    * @tparam F Base Functor
    */
  type PrecepteT[T, M, U, F[_], A] =
    PState[T, M, U] => F[(PState[T, M, U], A)]

  def liftPrecepteT[T, M, U, F[_]](
      F: MetaFunctor[F]): F ~~> PrecepteT[T, M, U, F, ?] =
    new (F ~~> PrecepteT[T, M, U, F, ?]) {
      def apply[A](fa: F[A]): PrecepteT[T, M, U, F, A] =
        (s: PState[T, M, U]) =>
          F.map(fa) { a: A =>
            ((s, a))
        }
    }

  implicit def precepteTInstances[T, M, U, F[_]](
      implicit F: MetaMonad[F],
      FE: MetaErrorEffect[Throwable, F],
      upd: PStateUpdater[T, M, U],
      FD: MetaDefer[F],
      S: MetaSemigroup[U])
    : MetaMonadPrecepteEffect[T, M, U, PrecepteT[T, M, U, F, ?]] =
    new PrecepteTInstances[T, M, U, F]

  class PrecepteTInstances[T, M, U, F[_]](implicit
                                          F: MetaMonad[F],
                                          FE: MetaErrorEffect[Throwable, F],
                                          FD: MetaDefer[F],
                                          upd: PStateUpdater[T, M, U],
                                          S: MetaSemigroup[U])
      extends MetaMonadPrecepteEffect[T, M, U, PrecepteT[T, M, U, F, ?]] {

    final type State = PState[T, M, U]
    final type G[X] = F[(State, X)]

    @inline final def pure[A](x: A): PrecepteT[T, M, U, F, A] =
      (s: State) => F.pure((s, x))

    @inline final def get: PrecepteT[T, M, U, F, PState[T, M, U]] =
      (s: State) => F.pure((s, s))

    @inline final def set(s: PState[T, M, U]): PrecepteT[T, M, U, F, Unit] =
      (_: State) => F.pure((s, ()))

    @inline final def defer[A](
        ga: => PrecepteT[T, M, U, F, A]): PrecepteT[T, M, U, F, A] =
      (s: State) => FD.defer(ga(s))

    @inline final def raiseError[A](e: Throwable): PrecepteT[T, M, U, F, A] =
      (_: State) => FE.raiseError(e)

    @inline final def catchError[A](sub: PrecepteT[T, M, U, F, A])(
        handler: Throwable => PrecepteT[T, M, U, F, A])
      : PrecepteT[T, M, U, F, A] =
      (s: State) =>
        FE.catchError(sub(s)) { e: Throwable =>
          handler(e)(s)
      }

    @inline final def map[A, B](fa: PrecepteT[T, M, U, F, A])(
        f: A => B): PrecepteT[T, M, U, F, B] =
      (s: State) => F.map(fa(s)) { case (s1, a) => (s1, f(a)) }

    @inline final def flatMap[A, B](pla: PrecepteT[T, M, U, F, A])(
        f: A => PrecepteT[T, M, U, F, B]): PrecepteT[T, M, U, F, B] =
      (s0: State) =>
        F.flatMap(pla(s0)) {
          case (s1, a) => f(a)(s1)
      }

    @inline final def ap[A, B](pla: PrecepteT[T, M, U, F, A])(
        plf: PrecepteT[T, M, U, F, A => B]): PrecepteT[T, M, U, F, B] =
      (s0: State) =>
        F.map2[(State, A), (State, A => B), (State, B)](
          pla(s0),
          plf(s0)
        ) {
          case ((sfa, a), (sff, g)) =>
            val s = upd.updateUnmanaged(upd.recombine(s0, sfa, sff),
                                        S.combine(sfa.unmanaged, sff.unmanaged))
            (s, g(a))
      }
  }
}
