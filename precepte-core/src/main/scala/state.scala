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

import shapeless._
import shapeless.ops.hlist.Selector

import scala.language.higherKinds

trait SubList[Super <: HList, S <: HList]
    extends DepFn1[Super]
    with Serializable {
  final type Out = S
}

object SubList {

  @inline implicit def hnilSubList[Super <: HList]: SubList[Super, HNil] =
    new SubList[Super, HNil] {
      def apply(s: Super) = HNil
    }

  @inline implicit def normalSubList[H, Super <: HList, S <: HList](
      implicit sel: Selector[Super, H],
      sub: SubList[Super, S]
  ): SubList[Super, H :: S] =
    new SubList[Super, H :: S] {
      def apply(s: Super): H :: sub.Out = sel(s) :: sub(s)
    }
}

trait SubListable[Super, Subsub] extends DepFn1[Super] with Serializable {
  type Out = Subsub
}

object SubListable {

  @inline implicit def sublistable[Super,
                                   Subsub,
                                   SuperHL <: HList,
                                   SubHL <: HList](
      implicit
      genSuper: Generic.Aux[Super, SuperHL],
      genSub: Generic.Aux[Subsub, SubHL],
      subList: SubList[SuperHL, SubHL]
  ): SubListable[Super, Subsub] =
    new SubListable[Super, Subsub] {
      def apply(s: Super): Subsub = genSub.from(subList(genSuper.to(s)))
    }

}

package object state {

  implicit final class UnifyStateUnit[Ta, MS, F[_], A](
      val p: Precepte[Ta, MS, Unit, F, A])
      extends AnyVal {
    @inline def unify[UMS2, R <: HList](
        pivot: PState[Ta, MS, UMS2]): Precepte[Ta, MS, UMS2, F, A] = {
      p.xmapState[UMS2]((_: Unit) => pivot.unmanaged, (_: UMS2) => ())
    }

  }

  implicit final class UnifyState[Ta, MS, UMS, F[_], A](
      val p: Precepte[Ta, MS, UMS, F, A])
      extends AnyVal {

    @inline def unify[UMS2,
                      HL <: HList,
                      C <: Coproduct,
                      HL2 <: HList,
                      C2 <: Coproduct](pivot: PState[Ta, MS, UMS2])(
        implicit
        sub: SubListable[UMS2, UMS]
    ): Precepte[Ta, MS, UMS2, F, A] = {
      p.xmapState[UMS2]((_: UMS) => pivot.unmanaged,
                        (_: UMS2) => sub(pivot.unmanaged))
    }
  }
}
