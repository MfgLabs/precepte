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


trait PrecepteUnapply[TCA, TC[_[_], _], F[_], Ta, MS, UMS, A]

object PrecepteUnapply {
  implicit def preUnapply[TC[_[_], _], F[_], Ta, MS, UMS, C, A] = new PrecepteUnapply[TC[({ type λ[α] = Precepte[Ta, MS, UMS, F, α] })#λ, A], TC, F, Ta, MS, UMS, A] { }
}

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
    // println(s"PT2: $p")
    p
  }
}


/*trait NoSI2712[TC[_[_]]] {
  type P[a] = Precepte[BaseTags, PIS0, PES0[Unit], Future, a]

  def mkTC: TC[({ type λ[t] = Precepte[BaseTags, PIS0, PES0[Unit], Future, t] })#λ]
}

object NoSI2712 {
  import scala.language.experimental.macros

  implicit def materialize[TC[_[_]]]: NoSI2712[TC] = macro NoSI2712Macros.materialize[TC]

}


trait Una[TCA, TC[_[_], _]] {
  type P[_]

  type A

  type T[x]

  def leibniz: TCA === T[A]

}

object Una {

  type Aux[TCA, TC[_[_], _], T0[_], A0] = Una[TCA, TC] {
    type A = A0
    type T[x] = T0[x]
  }

  implicit def una[TC[_[_], _], A0] = new Una[TC[({ type λ[α] = Precepte0[Future, Unit, α] })#λ, A0], TC] {
    type P[x] = Precepte0[Future, Unit, x]

    type A = A0

    type T[x] = TC[P, x]

    def leibniz = scalaz.Leibniz.refl
  }

}



trait MTCFromUna[TCA, M[_[_]]] {

  type T[x]

  type A

  lazy val MTC: M[T] = mkMTC

  def mkMTC: M[T]

  // def leibniz: TCA === T[A]

  def apply(tca: TCA): T[A]

}

trait NoSI2712T[TCA, TC[_[_], _], M[_[_]]] {

  lazy val una: Una[TCA, TC] = mkUna

  lazy val mtcFromUna: MTCFromUna[TCA, M] = mkMTCFromUna

  def mkUna: Una[TCA, TC]

  def mkMTCFromUna: MTCFromUna[TCA, M]


}

object NoSI2712T {
  import scala.language.experimental.macros

  implicit def materialize[TCA, TC[_[_], _], M[_[_]]]: NoSI2712T[TCA, TC, M] = macro NoSI2712Macros.materializeT[TCA, TC, M]

  def materializeFromUna[TCA, TC[_[_], _], M[_[_]], P[_], T[_], A]: MTCFromUna[TCA, M] =
    macro NoSI2712Macros.materializeFromUna[TCA, TC, M, P, T, A]

}


// MACROS
  def materialize[TC[_[_]]]
    (implicit
      lTag: WeakTypeTag[TC[Id]]
    ): Tree = {
    val lTpe = lTag.tpe

    val lTpt = c.internal.gen.mkAttributedRef(lTpe.typeSymbol)

    val p = q"""
      new _root_.com.mfglabs.precepte.NoSI2712[$lTpe] {

        def mkTC: $lTpt[({ type λ[t] =
          Precepte[
            _root_.com.mfglabs.precepte.BaseTags, _root_.com.mfglabs.precepte.PIS0,
            _root_.com.mfglabs.precepte.PES0[_root_.scala.Unit], _root_.scala.concurrent.Future,
            t] })#λ
          ] = _root_.shapeless.lazily[$lTpt[this.P]]

      }
    """

    println(s"P: $p")
    p
  }

  def materializeFromUna[TCA, TC[_[_], _], M[_[_]], P[_], T[_], A]
    (implicit
      tcaTag: WeakTypeTag[TCA],
      tcTag: WeakTypeTag[TC[Id, Int]],
      mTag: WeakTypeTag[M[Id]],
      pTag: WeakTypeTag[P[Int]],
      tTag: WeakTypeTag[T[Int]],
      aTag: WeakTypeTag[A]
    ): Tree = {
      val tcaTpe = tcaTag.tpe

      println(s"PTAG:${pTag.tpe}")

      val mTpe = mTag.tpe
      val mTpt = c.internal.gen.mkAttributedRef(mTpe.typeSymbol)

      val tTpe = tTag.tpe
      val tTpt = c.internal.gen.mkAttributedRef(tTpe.typeSymbol)

      val aTpe = aTag.tpe

      // type t[x] = unaE.T[x]
      val p = q"""new _root_.com.mfglabs.precepte.MTCFromUna[$tcaTpe, $mTpe] {

        type T[x] = $tTpt[x]

        type A = $aTpe

        def mkMTC: $mTpt[T] = _root_.shapeless.lazily[$mTpt[T]]

        def apply(tca: $tcaTpe): T[A] = tca
      }"""

      println(s"materializeFromUna: $p")
      p
  }

  def materializeT[TCA, TC[_[_], _], M[_[_]]]
    (implicit
      tcaTag: WeakTypeTag[TCA],
      tcTag: WeakTypeTag[TC[Id, Int]],
      mTag: WeakTypeTag[M[Id]]
    ): Tree = {
    val tcaTpe = tcaTag.tpe

    val tcTpe = tcTag.tpe
    val tcTpt = c.internal.gen.mkAttributedRef(tcTpe.typeSymbol)

    val mTpe = mTag.tpe
    val mTpt = c.internal.gen.mkAttributedRef(mTpe.typeSymbol)


    val p = q"""
      new _root_.com.mfglabs.precepte.NoSI2712T[$tcaTpe, $tcTpe, $mTpe] {
        self =>

        def mkUna: _root_.com.mfglabs.precepte.Una[$tcaTpe, $tcTpt] =
          _root_.shapeless.lazily[_root_.com.mfglabs.precepte.Una[$tcaTpe, $tcTpt]]

        def mkMTCFromUna = _root_.com.mfglabs.precepte.NoSI2712T.materializeFromUna[$tcaTpe, $tcTpt, $mTpt, una.P, una.T, una.A]

      }
    """
    println(s"PT: $p")
    p

  }

*/