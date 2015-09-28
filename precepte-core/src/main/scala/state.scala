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

import shapeless.{ Generic, HList, HNil, :: }
import shapeless.ops.hlist.Selector
import scalaz.Isomorphism.<=>
import scalaz.{Hoist, ~>}

import scala.language.implicitConversions
import scala.language.higherKinds


/** IsoState is meant to allow implicit conversions between Précepte with different UnmanagedState
  *
  * It decomposes the Unmanaged part of PState into a HList using Shapeless Generic and then create a few implicits:
  *   - implicits conversion from DefaultPre[F, UM, A] to DefaultPre[F, UM2, A] if there is an implicit <=>[UM, UM2]
  *   - implicits conversion from TC[({ type l[T] = DefaultPre[F, UM, T] })#l, A] to TC[({ type l[T] = DefaultPre[F, UM2, T] })#l, A] if there is an implicit <=>[UM, UM2]
  *   - some implicit Isomorphism (not real isomorphism theoretically speaking) between:
  *        - elements of HList & UM
  *        - elements of HList & Unit
  *        - UM & Unit
  *
  * Use it when you need to map Précepte having compatible UnmanagedState in the context of a Précepte such as:
  * {{{
  *   PreMP { s: ST[Ctx] => 
  *     val iso = IsoState(s); import iso._
  *     ...
  *   }
  * }}}
  *
  */
/*case class IsoState[Ta, MS, C](ss: PState[Ta, MS, C]) {

  // implicit def preMapState[F[_], UM, UM2, A](p: DefaultPre[F, UM, A])(implicit iso: <=>[UM, UM2]): DefaultPre[F, UM2, A] =
  //   p.xmapState(iso)

  // implicit def preMapState2[TC[_[_], _], F[_], UM, UM2, A](
  //   p: TC[({ type l[T] = DefaultPre[F, UM, T] })#l, A]
  // )(implicit 
  //   iso: <=>[UM, UM2],
  //   h: Hoist[TC]
  // ): TC[({ type l[T] = DefaultPre[F, UM2, T] })#l, A] = {
  //   type P[B] = DefaultPre[F, UM, B]
  //   type P2[B] = DefaultPre[F, UM2, B]
    
  //   val nat = new (P ~> P2) {
  //     def apply[D](pa: P[D]): P2[D] = pa.xmapState(iso)
  //   }
  //   h.hoist(nat).apply(p)
  // }

  implicit def isoSel[T, R <: HList](implicit gen: Generic.Aux[C, R], sel: Selector[R, T]): T <=> C = new (T <=> C) {
    def from: C => T = _ => sel(gen.to(ss.um))
    def to: T => C = _ => ss.um
  }

  implicit def isoSelUnit[T, R <: HList](implicit gen: Generic.Aux[C, R], sel: Selector[R, T]): Unit <=> T = new (Unit <=> T) {
    def from: T => Unit = _ => ()
    def to: Unit => T = _ => sel(gen.to(ss.um))
  }

  implicit def isoRUnit: Unit <=> C = new (Unit <=> C) {
    def from: C => Unit = _ => ()
    def to: Unit => C = _ => ss.um
  }
}*/

package object state {

  implicit class UnifyStateUnit[Ta, MS, F[_], A](val p: Precepte[Ta, MS, Unit, F, A]) extends AnyVal {
    def unify[UMS2, R <: HList](pivot: PState[Ta, MS, UMS2]): Precepte[Ta, MS, UMS2, F, A] = {
      val iso: Unit <=> UMS2 = new (Unit <=> UMS2) {
        def from: UMS2 => Unit = _ => ()
        def to: Unit => UMS2 = _ => pivot.um
      }
      
      p.xmapState[UMS2](iso)
    }

  }


  implicit class UnifyState[Ta, MS, UMS, F[_], A](val p: Precepte[Ta, MS, UMS, F, A]) extends AnyVal {
    def unify[UMS2, R <: HList](pivot: PState[Ta, MS, UMS2])(implicit gen: Generic.Aux[UMS2, R], sel: Selector[R, UMS]): Precepte[Ta, MS, UMS2, F, A] = {
      val iso: UMS <=> UMS2 = new (UMS <=> UMS2) {
        def from: UMS2 => UMS = _ => sel(gen.to(pivot.um))
        def to: UMS => UMS2 = _ => pivot.um
      }
      
      p.xmapState[UMS2](iso)
    }

  }

  // implicit def UnifyStateUnitImpl[Ta, MS, UMS2, F[_], A](p: Precepte[Ta, MS, Unit, F, A])(
  //   implicit pivot: PState[Ta, MS, UMS2]
  // ): Precepte[Ta, MS, UMS2, F, A] = {
  //   (new UnifyStateUnit(p)).unify(pivot)
  // }

  // implicit def UnifyStateImpl[Ta, MS, UMS, UMS2, F[_], A, R <: HList](p: Precepte[Ta, MS, UMS, F, A])(
  //   implicit pivot: PState[Ta, MS, UMS2], gen: Generic.Aux[UMS2, R], sel: Selector[R, UMS]
  // ) = {
  //   (new UnifyState(p)).unify(pivot)
  // }

}