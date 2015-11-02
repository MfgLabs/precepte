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

import scala.language.higherKinds
import scala.language.implicitConversions


trait *->*[F0[_]] {}

object *->* {
  implicit def fKindEv[F0[_]] = new *->*[F0] {}
}

trait *->*->*[F0[_, _]] {}
object *->*->* {
  implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}
}

// Not used but can be useful to hack SI-2172
case class SingletonOf[TC[_[_], _], U, V <: { type A; type T[_] }](
  widen: U { type A = V#A ; type T[x] = V#T[x] }
)

object SingletonOf {
  implicit def mkSingletonOf[TC[_[_], _], U <: { type A; type T[_] }](implicit
    u: U
  ): SingletonOf[TC, U, u.type] = SingletonOf(u)
}

/** Forces scalac to identify all the elements of the Precepte type... completely non generic and custom to Precepte type */
trait PrecepteUnapply[TCA, TC[_[_], _], F[_], Ta, MS, UMS, A]

object PrecepteUnapply {
  implicit def preUnapply[TC[_[_], _], F[_], Ta, MS, UMS, C, A] = new PrecepteUnapply[TC[({ type λ[α] = Precepte[Ta, MS, UMS, F, α] })#λ, A], TC, F, Ta, MS, UMS, A] { }
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

}

package object hk extends HK
