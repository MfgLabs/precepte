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

import cats.{Monad, Applicative, Functor, ~>}
import cats.kernel.Semigroup
import cats.data.{OptionT, EitherT}

import scala.language.higherKinds

package object corecats extends SubMeta {

  @inline implicit def precepteMonad[Ta, ManagedState, UnmanagedState, F[_]]
    : Monad[Precepte[Ta, ManagedState, UnmanagedState, F, ?]] =
    new Monad[Precepte[Ta, ManagedState, UnmanagedState, F, ?]] {
      override def pure[A](
          a: A): Precepte[Ta, ManagedState, UnmanagedState, F, A] =
        Return(a)
      override def map[A, B](
          m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(
          f: A => B): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.map(f)
      override def flatMap[A, B](
          m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(
          f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B])
        : Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.flatMap(f)

      override def ap[A, B](
          pab: Precepte[Ta, ManagedState, UnmanagedState, F, A => B])(
          pa: Precepte[Ta, ManagedState, UnmanagedState, F, A])
        : Precepte[Ta, ManagedState, UnmanagedState, F, B] = {
        Apply(pa, pab)
      }
      override def tailRecM[A, B](a: A)(
          f: A => Precepte[Ta, ManagedState, UnmanagedState, F, Either[A, B]])
        : Precepte[Ta, ManagedState, UnmanagedState, F, B] = {
        f(a).flatMap {
          case Left(result)  => tailRecM(result)(f)
          case Right(result) => pure(result)
        }
      }
    }

  @inline implicit def CatSemigroup[A](
      implicit sg: Semigroup[A]): MetaSemigroup[A] =
    new MetaSemigroup[A] {
      def combine(a: A, b: A): A = sg.combine(a, b)
    }

  @inline implicit def CatsMetaMonad[F[_]](
      implicit mo: Monad[F]): MetaMonad[F] =
    new MetaMonad[F] {
      override def pure[A](a: A): F[A] = mo.pure(a)
      override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
      override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
        mo.flatMap(fa)(f)
      override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = mo.ap(fab)(fa)
    }

  @inline implicit def CatsMetaNat[F[_], G[_]](implicit nat: F ~> G): F ~~> G =
    new (F ~~> G) {
      def apply[A](f: F[A]): G[A] = nat(f)
    }

  @inline implicit def CatsMetaIso[F[_], G[_]](implicit to0: F ~> G,
                                               from0: G ~> F): F <~~> G =
    new (F <~~> G) {
      def to[A](f: F[A]): G[A] = to0(f)
      def from[A](f: G[A]): F[A] = from0(f)
    }

  implicit final object optionHasHoist extends HasHoist[Option] {
    type T[F[_], A] = OptionT[F, A]
    def lift[F[_], A](f: F[Option[A]]): OptionT[F, A] = OptionT(f)
  }

  // implicit object streamingTHasHoist extends HasHoist[StreamingT] {
  //   type T[F[_], A] = StreamingT[F, A]
  //   def lift[F[_], A](f: F[StreamingT[A]]): StreamingT[F, A] = StreamingT.wait(f)
  // }

  implicit def xorHasHoist[A]
    : HasHoist.Aux[Either[A, ?], λ[(α[_], β) => EitherT[α, A, β]]] =
    new XorHasHoist[A]

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def future[Ta, M, U, A](ta: Ta)(λ: PState[Ta, M, U] => Future[A])(
      implicit ec: scala.concurrent.ExecutionContext)
    : Precepte[Ta, M, U, Future, Either[Throwable, A]] =
    Precepte(ta) { pa =>
      import cats.instances.future._
      Functor[Future]
        .map(λ(pa))(Right(_))
        .recover { case e => Left(e) }
    }

}

trait SubMeta {
  @inline implicit def CatsMetaFunctor[F[_]](
      implicit mo: Functor[F]): MetaFunctor[F] =
    new MetaFunctor[F] {
      override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
    }

  @inline implicit def CatsMetaApplicative[F[_]](
      implicit mo: Applicative[F]): MetaApplicative[F] =
    new MetaApplicative[F] {
      override def pure[A](a: A): F[A] = mo.pure(a)
      override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
      override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = mo.ap(fab)(fa)
    }
}
