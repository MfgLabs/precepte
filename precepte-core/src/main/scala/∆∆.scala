package utils

import scala.language.existentials
import scala.language.experimental.macros
import scala.language.higherKinds

import scala.annotation.{ StaticAnnotation, tailrec }
import scala.reflect.api.Universe
import scala.reflect.macros.{ blackbox, whitebox }


import shapeless.CaseClassMacros
import shapeless.Id

trait ∆∆[F[_], FR[_[_]]] {
  type R[t]

  lazy val fr: FR[R] = mkFrr

  // lazy val ff: FR[F] = mkFrf

  def mkFrr: FR[R]

  // def mkFrf: FR[F]
}

object ∆∆ extends ∆∆∆∆ with ∆∆∆∆∆ with ∆∆∆∆∆∆ {
  type Aux[F[_], FR[_[_]], R0[_]] = ∆∆[F, FR] { type R[t] = R0[t] }

  def apply[F[_], FR[_[_]]](implicit gen: ∆∆[F, FR]): Aux[F, FR, gen.R] = gen

  implicit def materialize22[T[_[_], _], FR[_[_]], F[_, _], I]: ∆∆[({type λ[A] = T[({type λ[A] = F[I, A]})#λ, A]})#λ, FR] =
    macro ∆∆∆.materialize22[T, FR, F, I]

}

trait ∆∆∆∆ {

  // implicit def materialize2[T[_[_], _], FR[_[_]], F[_]]: ∆∆[({type λ[A] = T[F, A]})#λ, FR] =
  //   macro ∆∆∆.materialize2[T, FR, F]


}

trait ∆∆∆∆∆ {

  implicit def materialize1[T[_, _], FR[_[_]], I]: ∆∆[({type λ[A] = T[I, A]})#λ, FR] =
    macro ∆∆∆.materialize1[T, FR, I]

}


trait ∆∆∆∆∆∆ {

  // implicit def materialize[T[_], FR[_[_]]]: ∆∆[T, FR] = macro ∆∆∆.materialize[T, FR]

}


class ∆∆∆(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._
  // import internal.constantType

  def materialize[T[_], FR[_[_]]](implicit tTag: WeakTypeTag[T[_]], frTag: WeakTypeTag[FR[Id]]): Tree = {
    val tpe = weakTypeOf[T[_]]
    println(s"tpe:$tpe tTag:$tTag")
    val frTpe = frTag.tpe.typeConstructor
    println(s"frTpe:$frTpe")
    val nme = TypeName(c.freshName)
    val clsName = TypeName(c.freshName())

    val tpeTpt = appliedTypTree1(tpe, param1(tpe), nme)

    // val reprTpt = reprTypTree1(tpe, nme)
    val frTpt = mkAttributedRef(frTpe)

    val r = q"""
      final class $clsName extends _root_.utils.∆∆[$tpe, $frTpe] {
        type R[$nme] = $tpeTpt

        def mkFrr: $frTpt[R] = shapeless.lazily[$frTpt[R]]

      }
      new $clsName()
    """

    println(r)
    r
  }


  def materialize1[T[_, _], FR[_[_]], I](implicit tTag: WeakTypeTag[T[_, _]], frTag: WeakTypeTag[FR[Id]], iTag: WeakTypeTag[I]): Tree = {
    val tpe = weakTypeOf[T[_, _]]
    println(s"tpe:$tpe tTag:$tTag")
    val tpeI = weakTypeOf[I]
    println(s"tpeI:$tpeI iTag:$iTag")
    val frTpe = frTag.tpe.typeConstructor
    println(s"frTpe:$frTpe")
    val nme = TypeName(c.freshName)
    val nme2 = TypeName(c.freshName)
    val nme3 = TypeName(c.freshName)
    val clsName = TypeName(c.freshName())

    // val tpeTpt = appliedTypTree1(tpe, param1(tpe), nme)
    // val tpeTpt = appliedType(tpe, tpeI, nme3)

    // val reprTpt = reprTypTree1(tpe, nme)
    val tpt = mkAttributedRef(tpe)
    val frTpt = mkAttributedRef(frTpe)

    val r = q"""{
      type $nme2[$nme3] = $tpt[$tpeI, $nme3]
      final class $clsName extends _root_.utils.∆∆[$nme2, $frTpe] {
        type R[$nme] = $nme2[$nme]

        def mkFrr: $frTpt[R] = shapeless.lazily[$frTpt[R]]

      }
      new $clsName()
    }
    """

    println(r)
    r
  }

  def materialize2[T[_[_], _], FR[_[_]], F[_]](implicit tTag: WeakTypeTag[T[F, _]], frTag: WeakTypeTag[FR[Id]], fTag: WeakTypeTag[F[_]]): Tree = {
    val tpe = weakTypeOf[T[F, _]]
    println(s"tpe:$tpe tTag:$tTag")
    val tpeF = weakTypeOf[F[_]]
    println(s"tpeF:$tpeF fTag:$fTag")
    val frTpe = frTag.tpe.typeConstructor
    println(s"frTpe:$frTpe")
    val nme = TypeName(c.freshName)
    val nme2 = TypeName(c.freshName)
    val nme3 = TypeName(c.freshName)
    val nme4 = TypeName(c.freshName)
    val nme5 = TypeName(c.freshName)
    val nme6 = TermName(c.freshName)
    val clsName = TypeName(c.freshName())

    // val tpeTpt = appliedTypTree1(tpe, param1(tpe), nme)
    // val tpeTpt = appliedType(tpe, tpeI, nme3)

    // val reprTpt = reprTypTree1(tpe, nme)
    val tpt = mkAttributedRef(tpe)
    val frTpt = mkAttributedRef(frTpe)
    val fTpt = mkAttributedRef(tpeF)

      //val $nme6 = shapeless.lazily[_root_.utils.∆∆[$tpeF, $frTpe]]
    val r = q"""{
      val $nme6 = shapeless.lazily[$frTpe[$tpeF]]
      type $nme4[$nme5] = $nme6.R[$nme5]
      type $nme2[$nme3] = $tpt[$nme4, $nme3]
      final class $clsName extends _root_.utils.∆∆[$nme2, $frTpe] {
        type R[$nme] = $nme2[$nme]

        def mkFrr: $frTpt[R] = shapeless.lazily[$frTpt[R]]

      }
      new $clsName()
    }
    """

    println(r)
    r
  }

  def materialize22[T[_[_], _], FR[_[_]], F[_, _], I](implicit tTag: WeakTypeTag[T[Id, _]], frTag: WeakTypeTag[FR[Id]], fTag: WeakTypeTag[F[_, _]], iTag: WeakTypeTag[I]): Tree = {
    val tpe = weakTypeOf[T[Id, _]]
    println(s"tpe:$tpe tTag:$tTag")
    val tpeF = weakTypeOf[F[_, _]]
    println(s"tpeF:$tpeF fTag:$fTag")
    val frTpe = frTag.tpe.typeConstructor
    println(s"frTpe:$frTpe")
    val tpeI = weakTypeOf[I]
    println(s"tpeI:$tpeI iTag:$iTag")

    val nme = TypeName(c.freshName)
    val nme2 = TypeName(c.freshName)
    val nme3 = TypeName(c.freshName)
    val nme4 = TypeName(c.freshName)
    val nme5 = TypeName(c.freshName)
    val clsName = TypeName(c.freshName())

    // val tpeTpt = appliedTypTree1(tpe, param1(tpe), nme)
    // val tpeTpt = appliedType(tpe, tpeI, nme3)

    // val reprTpt = reprTypTree1(tpe, nme)
    val tpt = mkAttributedRef(tpe)
    val frTpt = mkAttributedRef(frTpe)
    val fTpt = mkAttributedRef(tpeF)

    val r = q"""{
      type $nme4[$nme5] = $fTpt[$tpeI, $nme5]
      type $nme2[$nme3] = $tpt[$nme4, $nme3]
      final class $clsName extends _root_.utils.∆∆[$nme2, $frTpe] {
        type R[$nme] = $nme2[$nme]

        def mkFrr: $frTpt[R] = shapeless.lazily[$frTpt[R]]

      }
      new $clsName()
    }
    """

    println(r)
    r
  }
}


trait UnLambda1[F[_]] {
  type R[t]
}

object UnLambda1 {
  type Aux[F[_], R[_]] = UnLambda1[F] { type R }

  def apply[F[_]](implicit gen: UnLambda1[F]) = gen

  implicit def un21[F[_, _], I] = new UnLambda1[({type λ[A] = F[I, A]})#λ] {
    type R[t] = F[I, t]
  }
  implicit def un22[F[_, _], I] = new UnLambda1[({type λ[A] = F[A, I]})#λ] {
    type R[t] = F[t, I]
  }

  implicit def unT12[T[_[_], _], F[_, _], I](implicit fl : UnLambda1[({type λ[A] = F[I, A]})#λ]) =
    new UnLambda1[({type λ[A] = T[({type λ[A] = F[I, A]})#λ, A]})#λ] {
      type R[t] = T[fl.R, t]
    }
}

trait GenericMat[F[_], FR[_[_]]] {
  lazy val unl: UnLambda1[F] = mkUnl
  def mkUnl: UnLambda1[F]

  type R[t]

  lazy val fr: FR[R] = mkFrr
  def mkFrr: FR[R]
}

object GenericMat {
  type Aux[F[_], FR[_[_]], R0[_]] = GenericMat[F, FR] { type R[t] = R0[t] }

  def apply[F[_], FR[_[_]]](implicit gen: GenericMat[F, FR]): Aux[F, FR, gen.R] = gen

  implicit def materialize[F[_], FR[_[_]]]: GenericMat[F, FR] = 
    macro GenericMatMacros.materialize[F, FR]

}

class GenericMatMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._
  // import internal.constantType

  def materialize[T[_], FR[_[_]]](implicit tTag: WeakTypeTag[T[_]], frTag: WeakTypeTag[FR[Id]]): Tree = {
    val tpe = weakTypeOf[T[_]]
    println(s"tpe:$tpe tTag:$tTag")
    val frTpe = frTag.tpe.typeConstructor
    println(s"frTpe:$frTpe")
    val nme = TypeName(c.freshName)
    val unl = TermName(c.freshName)
    val clsName = TypeName(c.freshName())

    val tpeTpt = appliedTypTree1(tpe, param1(tpe), nme)

    val frTpt = mkAttributedRef(frTpe)

        // def mkUnl = _root_.shapeless.lazily[UnLambda1[$tpe]]
    val r = q"""
      final class $clsName extends _root_.utils.GenericMat[$tpe, $frTpe] {

        type R[$nme] = $tpe[$nme]
        
        def mkFrr: $frTpt[R] = shapeless.lazily[$frTpt[R]]

      }
      new $clsName()
    """

    println(r)
    r
  }
}
