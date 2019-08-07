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
import cats.{Applicative, Functor, Monad, MonadError, ~>}
import cats.kernel.Semigroup
import cats.data.{EitherT, OptionT}
import cats.mtl.MonadState

import scala.language.higherKinds

package object corecats extends SubMeta {

  @inline implicit def precepteMonad[T, M, U, F[_]]
    : (MonadError[Precepte[T, M, U, F, ?], Throwable]
      with MonadState[Precepte[T, M, U, F, ?], PState[T, M, U]]
      with cats.Defer[Precepte[T, M, U, F, ?]]) = {
    class C
        extends MonadError[Precepte[T, M, U, F, ?], Throwable]
        with MonadState[Precepte[T, M, U, F, ?], PState[T, M, U]]
        with cats.Defer[Precepte[T, M, U, F, ?]] {

      final type precepte[A] = Precepte[T, M, U, F, A]
      final type instrumentStep = SubStepInstrumentation[T, M, U, F]
      final type state = PState[T, M, U]

      final val monad: Monad[Precepte[T, M, U, F, ?]] = this

      @inline final def inspect[A](
          f: PState[T, M, U] => A): Precepte[T, M, U, F, A] =
        get.map(f)

      @inline final def modify(
          f: PState[T, M, U] => PState[T, M, U]): precepte[Unit] =
        flatMap(get)(f.andThen(set))

      @inline final def pure[A](x: A): precepte[A] =
        Precepte.pure[T, M, U, F, A](x)
      @inline final def defer[A](ga: => precepte[A]): precepte[A] =
        Precepte.defer[T, M, U, F, A](ga)

      @inline final def get: precepte[PState[T, M, U]] =
        Precepte.get[T, M, U, F]
      @inline final def set(s: PState[T, M, U]): precepte[Unit] =
        Precepte.set[T, M, U, F](s)

      @inline final def raiseError[A](e: Throwable): precepte[A] =
        Precepte.raiseError(e)

      @inline final override def map[A, B](fa: precepte[A])(
          f: A => B): precepte[B] =
        fa.map(f)

      @inline final def flatMap[A, B](fa: precepte[A])(
          f: A => precepte[B]): precepte[B] =
        fa.flatMap(f)
      @inline final override def ap[A, B](ff: precepte[A => B])(
          fa: precepte[A]): precepte[B] =
        fa.ap(ff)

      @inline final def tailRecM[A, B](a: A)(
          f: A => Precepte[T, M, U, F, Either[A, B]])
        : Precepte[T, M, U, F, B] = {
        f(a).flatMap {
          case Left(result)  => tailRecM(result)(f)
          case Right(result) => pure(result)
        }
      }

      @inline final def handleErrorWith[A](fa: precepte[A])(
          f: Throwable => precepte[A]): precepte[A] =
        Precepte.catchError(fa)(f)
    }

    new C
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
      def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
    }

  @inline implicit def CatsMetaApplicative[F[_]](
      implicit mo: Applicative[F]): MetaApplicative[F] =
    new MetaApplicative[F] {
      def pure[A](a: A): F[A] = mo.pure(a)
      def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
      def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = mo.ap(fab)(fa)
    }
}
