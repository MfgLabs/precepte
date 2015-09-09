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

import scalaz._
import scala.language.higherKinds
import scala.language.implicitConversions

import com.mfglabs.precepte._


package object precepte {

  implicit def toUnapply[TCA, TC[_[_], _], M[_[_]], F[_], Ta, MS, UMS, A0](
    implicit
      una2: PrecepteUnapply[TCA, TC, F, Ta, MS, UMS, A0],
      nosi: PrecepteHackSI2712[TCA, TC, M, F, Ta, MS, UMS, A0]
  ) = new Unapply[M, TCA] {
    type M[x] = nosi.T[x]
    type A = A0

    def TC = nosi.MTC

    def leibniz = nosi.leibniz
  }

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
