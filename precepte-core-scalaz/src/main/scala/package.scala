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

import scala.concurrent.Future

import scalaz.{
  Monad,
  Applicative,
  Functor,
  Unapply,
  Semigroup,
  ~>,
  \/,
  \/-,
  -\/,
  EitherT,
  OptionT,
  ListT
}
import scalaz.Isomorphism.<~>

import scala.language.higherKinds

package object corescalaz extends SubMeta {

  @inline implicit def precepteMonad[Ta, ManagedState, UnmanagedState, F[_]]
    : Monad[Precepte[Ta, ManagedState, UnmanagedState, F, ?]] =
    new Monad[Precepte[Ta, ManagedState, UnmanagedState, F, ?]] {
      override def point[A](
          a: => A): Precepte[Ta, ManagedState, UnmanagedState, F, A] =
        Pure(a)
      override def map[A, B](
          m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(
          f: A => B): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.map(f)
      override def bind[A, B](
          m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(
          f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B])
        : Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.flatMap(f)

      override def ap[A, B](
          pa: => Precepte[Ta, ManagedState, UnmanagedState, F, A])(
          pab: => Precepte[Ta, ManagedState, UnmanagedState, F, A => B])
        : Precepte[Ta, ManagedState, UnmanagedState, F, B] = {
        Apply(pa, pab)
      }
    }

  @inline implicit def ScalazSemigroup[A](
      implicit sg: Semigroup[A]): MetaSemigroup[A] =
    new MetaSemigroup[A] {
      def combine(a: A, b: A): A = sg.append(a, b)
    }

  @inline implicit def ScalazMetaMonad[F[_]](
      implicit mo: Monad[F]): MetaMonad[F] =
    new MetaMonad[F] {
      override def pure[A](a: A): F[A] = mo.point(a)
      override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
      override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = mo.bind(fa)(f)
      override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = mo.ap(fa)(fab)
    }

  @inline implicit def ScalazMetaNat[F[_], G[_]](
      implicit nat: F ~> G): F ~~> G =
    new (F ~~> G) {
      def apply[A](f: F[A]): G[A] = nat(f)
    }

  @inline implicit def ScalazMetaIso[F[_], G[_]](
      implicit iso: F <~> G): F <~~> G =
    new (F <~~> G) {
      def to[A](f: F[A]): G[A] = iso.to(f)
      def from[A](f: G[A]): F[A] = iso.from(f)
    }

  /** allows to unapply a Precepte into a F[A] */
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  @inline implicit def toClassicUnapply[M0[_[_]], Ta, MS, C, F[_], A0](
      implicit m: M0[Precepte[Ta, MS, C, F, ?]])
    : Unapply[M0, Precepte[Ta, MS, C, F, A0]] =
    new Unapply[M0, Precepte[Ta, MS, C, F, A0]] {
      type M[x] = Precepte[Ta, MS, C, F, x]
      type A = A0

      def TC = m

      def leibniz = scalaz.Leibniz.refl
    }

  /**
    * A custom typeclass allowing to go around higher-kind type unification issues in scalac when using Monad Transformers + Precepte
    */
  @inline implicit def toTCUnapply[TCA,
                                   TC[_[_], _],
                                   M[_[_]],
                                   F[_],
                                   Ta,
                                   MS,
                                   UMS,
                                   A0](
      implicit nosi: PrecepteHackSI2712[TCA, TC, M, F, Ta, MS, UMS, A0]
  ): Unapply[M, TCA] =
    new Unapply[M, TCA] {
      type M[x] = nosi.T[x]
      type A = A0

      def TC = nosi.MTC

      def leibniz = nosi.leibniz
    }

  implicit object optionHasHoist extends HasHoist[Option] {
    type T[F[_], A] = OptionT[F, A]
    def lift[F[_], A](f: F[Option[A]]): OptionT[F, A] = OptionT.optionT[F](f)
  }

  implicit object listHasHoist extends HasHoist[List] {
    type T[F[_], A] = ListT[F, A]
    def lift[F[_], A](f: F[List[A]]): ListT[F, A] = ListT.apply(f)
  }

  implicit def eitherHasHoist[A]
    : HasHoist.Aux[A \/ ?, λ[(α[_], β) => EitherT[α, A, β]]] =
    new EitherHasHoist[A]

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def future[Tag, M, U, A](tag: Tag)(λ: PState[Tag, M, U] => Future[A])(
      implicit ec: scala.concurrent.ExecutionContext)
    : Precepte[Tag, M, U, Future, \/[Throwable, A]] =
    Precepte(tag) { pa =>
      import scalaz.std.scalaFuture._
      Functor[Future]
        .map(λ(pa))(\/-.apply(_))
        .recover { case e => -\/(e) }
    }
}

trait SubMeta {

  @inline implicit final def ScalazMetaFunctor[F[_]](
      implicit mo: Functor[F]): MetaFunctor[F] =
    new MetaFunctor[F] {
      override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
    }

  @inline implicit final def ScalazMetaApplicative[F[_]](
      implicit mo: Applicative[F]): MetaApplicative[F] =
    new MetaApplicative[F] {
      override def pure[A](a: A): F[A] = mo.point(a)
      override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
      override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = mo.ap(fa)(fab)
    }
}
