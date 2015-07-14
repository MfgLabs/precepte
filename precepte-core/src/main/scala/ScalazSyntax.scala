package scalaz
package syntax
package ext

import scalaz._
import scala.language.higherKinds
import scala.language.implicitConversions
import shapeless.WrappedOrphan

import com.mfglabs.precepte._

object MonadPlusOpsExt {

  implicit def toMonadPlusOps[TCA, TC[_[_], _], F[_], Tags, MS, UMS, A](tca: TCA)(
    implicit
      una2: PreUnapply[TCA, TC, F, Tags, MS, UMS, A],
      nosi: PreHackSI2712[TCA, TC, MonadPlus, F, Tags, MS, UMS, A]
  ) = new MonadPlusOps[nosi.T, A](nosi.leibniz(tca))(nosi.MTC)
  
}