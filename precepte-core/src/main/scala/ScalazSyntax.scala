package scalaz
package syntax
package ext

import scalaz._
import scala.language.higherKinds
import scala.language.implicitConversions
import shapeless.WrappedOrphan

import com.mfglabs.precepte._

object MonadPlusOpsExt {

  // implicit def toMonadPlusOps[TCA, TC[_[_], _], F[_], Tags, MS, UMS, A](tca: TCA)(
  //   implicit
  //     una2: PrecepteUnapply[TCA, TC, F, Tags, MS, UMS, A],
  //     nosi: PrecepteHackSI2712[TCA, TC, MonadPlus, F, Tags, MS, UMS, A]
  // ) = new MonadPlusOps[nosi.T, A](nosi.leibniz(tca))(nosi.MTC)

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