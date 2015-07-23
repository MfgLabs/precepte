package com.mfglabs

import scalaz._
import scala.language.higherKinds
import scala.language.implicitConversions
import shapeless.WrappedOrphan

import com.mfglabs.precepte._

package precepte {
  case class ManagedState0[E <: Env, T <: Tags](env: E, span: Span, path: Call.Path[T], ids: PIdSeries = PIdStream())
}

package object precepte {

  type PIS0 = ManagedState0[BaseEnv, BaseTags]
  type PST0[C] = PState[BaseTags, PIS0, C]

  object PST0 {
    def apply[C](span: Span, env: BaseEnv, path: Call.Path[BaseTags], value: C): PST0[C] =
      PState[BaseTags, PIS0, C] (
        ManagedState0(env, span, path),
        value
      )
  }

  implicit def updater0[C] = new PStateUpdater[BaseTags, PIS0, C] {
    def appendTags(s: PST0[C], tags: BaseTags) = {
      val (id, next) = s.managed.ids.run()
      val is0 = ManagedState0(s.managed.env, s.managed.span, s.managed.path :+ Call(id, tags), next)
      s.copy(managed = is0)
    }
    def updateUnmanaged(s: PST0[C], unmanaged: C): PST0[C] = s.copy(unmanaged = unmanaged)
  }

  type Precepte0[F[_], C, A] = Precepte[BaseTags, PIS0, C, F, A]

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