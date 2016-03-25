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

import scalaz.{ Monad, Applicative, Functor, Unapply, Semigroup, ~>, \/, \/-, -\/, IndexedStateT, StateT, EitherT, OptionT, ListT }
import scalaz.Isomorphism.<~>
import scalaz.syntax.monad._

import scala.language.higherKinds
import scala.annotation.tailrec

package object corescalaz extends SubMeta {

  implicit def precepteMonad[Ta, ManagedState, UnmanagedState, F[_]] =
    new Monad[({ type λ[α] = Precepte[Ta, ManagedState, UnmanagedState, F, α] })#λ] {
      override def point[A](a: => A): Precepte[Ta, ManagedState, UnmanagedState, F, A] =
        Return(a)
      override def map[A, B](m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(f: A => B): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.map(f)
      override def bind[A, B](m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.flatMap(f)

      override def ap[A, B](pa: => Precepte[Ta, ManagedState, UnmanagedState, F, A])(pab: => Precepte[Ta, ManagedState, UnmanagedState, F, A => B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] = {
        Apply(pa, pab)
      }
    }

  implicit def ScalazSemigroup[A](implicit sg: Semigroup[A]) = new MetaSemigroup[A] {
    def combine(a: A, b: A): A = sg.append(a, b)
  }

  implicit def ScalazMetaMonad[F[_]](implicit mo: Monad[F]) = new MetaMonad[F] {
    override def pure[A](a: A): F[A] = mo.point(a)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = mo.bind(fa)(f)
    override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = mo.ap(fa)(fab)
  }

  implicit def ScalazMetaNat[F[_], G[_]](implicit nat: F ~> G) = new ~~>[F, G] {
    def apply[A](f: F[A]): G[A] = nat(f)
  }

  implicit def ScalazMetaIso[F[_], G[_]](implicit iso: F <~> G) = new <~~>[F, G] {
    def to[A](f: F[A]): G[A] = iso.to(f)
    def from[A](f: G[A]): F[A] = iso.from(f)
  }

  /** allows to unapply a Precepte into a F[A] */
  implicit def toClassicUnapply[M0[_[_]], Ta, MS, C, F[_], A0](implicit m: M0[({ type λ[α] = Precepte[Ta, MS, C, F, α] })#λ]) = new Unapply[M0, Precepte[Ta, MS, C, F, A0]] {
    type M[x] = Precepte[Ta, MS, C, F, x]
    type A = A0

    def TC = m

    def leibniz = scalaz.Leibniz.refl
  }

  /**
    * A custom typeclass allowing to go around higher-kind type unification issues in scalac when using Monad Transformers + Precepte
    */
  implicit def toTCUnapply[TCA, TC[_[_], _], M[_[_]], F[_], Ta, MS, UMS, A0](
    implicit
      una2: PrecepteUnapply[TCA, TC, F, Ta, MS, UMS, A0],
      nosi: PrecepteHackSI2712[TCA, TC, M, F, Ta, MS, UMS, A0]
  ) = new Unapply[M, TCA] {
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

  implicit def eitherHasHoist[A]: HasHoist.Aux[({ type λ[α] = A \/ α })#λ, ({ type λ[F[_], B] = EitherT[F, A, B] })#λ] = new EitherHasHoist[A]

  def future[Ta, M, U, A](ta: Ta)(λ: Precepte[Ta, M, U, Future, A]#S => Future[A])(implicit func: Functor[Future], ec: scala.concurrent.ExecutionContext): Precepte[Ta, M, U, Future, \/[Throwable, A]] =
    Precepte(ta){ pa =>
      func.map(λ(pa))(\/-.apply(_))
        .recover{ case e => -\/(e) }
    }

}

trait SubMeta {

  implicit def ScalazMetaFunctor[F[_]](implicit mo: Functor[F]) = new MetaFunctor[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
  }

  implicit def ScalazMetaApplicative[F[_]](implicit mo: Applicative[F]) = new MetaApplicative[F] {
    override def pure[A](a: A): F[A] = mo.point(a)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = mo.map(fa)(f)
    override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = mo.ap(fa)(fab)
  }



}