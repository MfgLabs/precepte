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
package corescalaz

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.{ EitherT, \/, Unapply }


class EitherHasHoist[A] extends HasHoist[({ type λ[α] = A \/ α })#λ] {
  type T[F[_], B] = EitherT[F, A, B]
  def lift[F[_], B](f: F[A \/ B]): EitherT[F, A, B] = EitherT.apply(f)
}


trait *->*[F0[_]] {}

object *->* {
  implicit def fKindEv[F0[_]] = new *->*[F0] {}
}

trait *->*->*[F0[_, _]] {}
object *->*->* {
  implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}
}

/**
  * Helpers for Precepte wrapped in Monad Transformers (OptionT, ListT, EitherT)
  */
trait HK {

  /**
    * Finds the right Monad Transformer (OptionT, ListT, EitherT) wrapped in a precepte using HasHoist typeclass
    * and lifts the Precepte into a MonadTransformer instance. 
    */
  def trans[Ta, ManagedState, UnmanagedState, F[_], G[_]: *->*, A](
    m: Precepte[Ta, ManagedState, UnmanagedState, F, G[A]]
  )(implicit hh: HasHoist[G]): hh.T[({ type λ[α] = Precepte[Ta, ManagedState, UnmanagedState, F, α] })#λ, A] =
    hh.lift[({ type λ[α] = Precepte[Ta, ManagedState, UnmanagedState, F, α] })#λ, A](m)

  def trans[Ta, ManagedState, UnmanagedState, F[_], G[_, _]: *->*->*, A, B](
    m: Precepte[Ta, ManagedState, UnmanagedState, F, G[A, B]]
  )(implicit hh: HasHoist[({ type λ[α] = G[A, α] })#λ]): hh.T[({ type λ[α] = Precepte[Ta, ManagedState, UnmanagedState, F, α] })#λ, B] = {
    type λ[α] = G[A, α]
    trans[Ta, ManagedState, UnmanagedState, F, λ, B](m)(new *->*[λ] {}, hh)
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
}

package object hk extends HK
