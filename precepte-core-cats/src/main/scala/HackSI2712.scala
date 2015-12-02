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
package corecats

import scala.annotation.{ StaticAnnotation, tailrec }
import scala.reflect.api.Universe
import scala.reflect.macros.{ blackbox, whitebox }
import scala.concurrent.Future

import cats.Eq
import cats.data.{ OptionT, XorT, Xor, StreamingT }

import scala.language.higherKinds
import scala.language.existentials


class XorHasHoist[A] extends HasHoist[({ type λ[α] = Xor[A, α] })#λ] {
  type T[F[_], B] = XorT[F, A, B]
  def lift[F[_], B](f: F[Xor[A, B]]): XorT[F, A, B] = XorT.apply(f)
}

/** The horrible cludge to go around SI-2712 */
trait PrecepteHackSI2712[TCA, TC[_[_], _], M[_[_]], F[_], Ta, MS, UMS, A] {

  type P[_]

  type T[_]

  lazy val MTC: M[T] = mkMTC

  def mkMTC: M[T]

  def subst: TCA => T[A]

}

object PrecepteHackSI2712 {
  import scala.language.experimental.macros

  implicit def materialize[TCA, TC[_[_], _], M[_[_]], F[_], Ta, MS, UMS, A]: PrecepteHackSI2712[TCA, TC, M, F, Ta, MS, UMS, A] = macro NoSI2712Macros.materialize[TCA, TC, M, F, Ta, MS, UMS, A]

}

class NoSI2712Macros(val c: whitebox.Context) {
  import c.universe._
  import shapeless.Id

  def materialize[TCA, TC[_[_], _], M[_[_]], F[_], Ta, MS, UMS, A]
    (implicit
      tcaTag: WeakTypeTag[TCA],
      tcTag: WeakTypeTag[TC[Id, Int]],
      mTag: WeakTypeTag[M[Id]],
      fTag: WeakTypeTag[F[Int]],
      tagsTag: WeakTypeTag[Ta],
      msTag: WeakTypeTag[MS],
      umsTag: WeakTypeTag[UMS],
      aTag: WeakTypeTag[A]
    ): Tree = {
    val tcaTpe = tcaTag.tpe

    val tcTpe = tcTag.tpe
    val tcTpt = c.internal.gen.mkAttributedRef(tcTpe.typeSymbol)

    val mTpe = mTag.tpe
    val mTpt = c.internal.gen.mkAttributedRef(mTpe.typeSymbol)

    val fTpe = fTag.tpe
    val fTpt = c.internal.gen.mkAttributedRef(fTpe.typeSymbol)

    val tagsTpe = tagsTag.tpe
    val msTpe = msTag.tpe
    val umsTpe = umsTag.tpe

    val aTpe = aTag.tpe

    val nme = TypeName(c.freshName)
    val nme2 = TypeName(c.freshName)

    val p = q"""
      new _root_.com.mfglabs.precepte.corecats.PrecepteHackSI2712[$tcaTpe, $tcTpe, $mTpe, $fTpe, $tagsTpe, $msTpe, $umsTpe, $aTpe] {
        self =>

        type P[$nme] = _root_.com.mfglabs.precepte.Precepte[$tagsTpe, $msTpe, $umsTpe, $fTpt, $nme]

        type T[$nme2] = $tcTpt[P, $nme2]

        def mkMTC = _root_.shapeless.lazily[$mTpt[T]]

        def subst: $tcaTpe => T[$aTpe] = tca => tca
      }
    """
    p
  }
}
