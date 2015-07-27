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

}