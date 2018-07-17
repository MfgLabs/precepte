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

import cats.{ Monad, Applicative, Functor, ~> }
import cats.kernel.Semigroup
import cats.data.{ OptionT, EitherT }

import scala.language.higherKinds

package object corecats extends SubMeta {

  implicit def precepteMonad[Ta, ManagedState, UnmanagedState, F[_]] =
    new Monad[({ type λ[α] = Precepte[Ta, ManagedState, UnmanagedState, F, α] })#λ] {
      override def pure[A](a: A): Precepte[Ta, ManagedState, UnmanagedState, F, A] =
        Return(a)
      override def map[A, B](m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(f: A => B): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.map(f)
      override def flatMap[A, B](m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.flatMap(f)

      override def ap[A, B](pab: Precepte[Ta, ManagedState, UnmanagedState, F, A => B])(pa: Precepte[Ta, ManagedState, UnmanagedState, F, A]): Precepte[Ta, ManagedState, UnmanagedState, F, B] = {
        Apply(pa, pab)
      }
      override def tailRecM[A, B](a: A)(f: A => Precepte[Ta, ManagedState, UnmanagedState, F, Either[A,B]]): Precepte[Ta, ManagedState, UnmanagedState, F, B] = {
        f(a).fastFlatMap {
          case Left(result) => tailRecM(result)(f)
          case Right(result) => pure(result)
        }
      }
    }

  implicit def CatSemigroup[A](implicit sg: Semigroup[A]) = new MetaSemigroup[A] {
    def combine(a: A, b: A): A = sg.combine(a, b)
  }

  implicit def CatsMetaMonad[F[_]](implicit mo: Monad[F]) = new MetaMonad[F] {
    override def pure[A](a: A): F[A] = mo.pure(a)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = mo.flatMap(fa)(f)
    override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = mo.ap(fab)(fa)
  }

  implicit def CatsMetaNat[F[_], G[_]](implicit nat: F ~> G) = new ~~>[F, G] {
    def apply[A](f: F[A]): G[A] = nat(f)
  }

  implicit def CatsMetaIso[F[_], G[_]](implicit to0: F ~> G, from0: G ~> F) = new <~~>[F, G] {
    def to[A](f: F[A]): G[A] = to0(f)
    def from[A](f: G[A]): F[A] = from0(f)
  }

  implicit object optionHasHoist extends HasHoist[Option] {
    type T[F[_], A] = OptionT[F, A]
    def lift[F[_], A](f: F[Option[A]]): OptionT[F, A] = OptionT(f)
  }

  // implicit object streamingTHasHoist extends HasHoist[StreamingT] {
  //   type T[F[_], A] = StreamingT[F, A]
  //   def lift[F[_], A](f: F[StreamingT[A]]): StreamingT[F, A] = StreamingT.wait(f)
  // }

  implicit def xorHasHoist[A]: HasHoist.Aux[({ type λ[α] = Either[A, α] })#λ, ({ type λ[F[_], B] = EitherT[F, A, B] })#λ] = new XorHasHoist[A]

  def future[Ta, M, U, A](ta: Ta)(λ: Precepte[Ta, M, U, Future, A]#S => Future[A])(implicit func: Functor[Future], ec: scala.concurrent.ExecutionContext): Precepte[Ta, M, U, Future, Either[Throwable, A]] =
    Precepte(ta){ pa =>
      func.map(λ(pa))(Right(_))
        .recover{ case e => Left(e) }
    }

}

trait SubMeta {

  implicit def CatsMetaFunctor[F[_]](implicit mo: Functor[F]) = new MetaFunctor[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
  }

  implicit def CatsMetaApplicative[F[_]](implicit mo: Applicative[F]) = new MetaApplicative[F] {
    override def pure[A](a: A): F[A] = mo.pure(a)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
    override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = mo.ap(fab)(fa)
  }

}
