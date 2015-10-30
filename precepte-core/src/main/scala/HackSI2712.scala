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

import scala.language.existentials

import scala.annotation.{ StaticAnnotation, tailrec }
import scala.reflect.api.Universe
import scala.reflect.macros.{ blackbox, whitebox }

import scala.language.higherKinds

import scala.concurrent.Future

import scalaz.Leibniz.{===, refl}


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

/** The horrible cludge to go around SI-2712 */
trait PrecepteHackSI2712[TCA, TC[_[_], _], M[_[_]], F[_], Ta, MS, UMS, A] {

  type P[_]

  type T[_]

  lazy val MTC: M[T] = mkMTC

  def mkMTC: M[T]

  def leibniz: TCA === T[A]

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
      new _root_.com.mfglabs.precepte.PrecepteHackSI2712[$tcaTpe, $tcTpe, $mTpe, $fTpe, $tagsTpe, $msTpe, $umsTpe, $aTpe] {
        self =>

        type P[$nme] = _root_.com.mfglabs.precepte.Precepte[$tagsTpe, $msTpe, $umsTpe, $fTpt, $nme]

        type T[$nme2] = $tcTpt[P, $nme2]

        def mkMTC = _root_.shapeless.lazily[$mTpt[T]]

        def leibniz: _root_.scalaz.Leibniz.===[$tcaTpe, T[$aTpe]] = _root_.scalaz.Leibniz.refl
      }
    """
    p
  }
}