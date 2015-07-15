package com.mfglabs

import scalaz._
import scala.language.higherKinds
import scala.language.implicitConversions
import shapeless.WrappedOrphan

import com.mfglabs.precepte._


package object precepte {

  case class ManagedState0[T <: Tags](path: Call.Path[T], ids: PIdSeries = PIdStream())
  case class UnManagedState0[E <: Env, C](span: Span, env: E, value: C)

  type PIS0 = ManagedState0[BaseTags]
  type PES0[C] = UnManagedState0[BaseEnv, C]
  type PST0[C] = PState[BaseTags, PIS0, PES0[C]]

  object PST0 {
    def apply[C](span: Span, env: BaseEnv, path: Call.Path[BaseTags], value: C): PST0[C] =
      PState[BaseTags, PIS0, PES0[C]] (
        ManagedState0(path),
        UnManagedState0(span, env, value)
      )
  }

  implicit def updater0[C] = new PStateUpdater[BaseTags, PIS0, PES0[C]] {
    def appendTags(s: PST0[C], tags: BaseTags) = {
      val (id, next) = s.managed.ids.run()
      val is0 = ManagedState0(s.managed.path :+ Call(id, tags), next)
      s.copy(managed = is0)
    }
    def updateUnmanaged(s: PST0[C], unmanaged: PES0[C]): PST0[C] = s.copy(unmanaged = unmanaged)
  }

  type Precepte0[F[_], C, A] = Precepte[BaseTags, PIS0, PES0[C], F, A]

  implicit def toUnapply[TCA, TC[_[_], _], M[_[_]], F[_], Tags, MS, UMS, A0](
    implicit
      una2: PrecepteUnapply[TCA, TC, F, Tags, MS, UMS, A0],
      nosi: PrecepteHackSI2712[TCA, TC, M, F, Tags, MS, UMS, A0]
  ) = new Unapply[M, TCA] {
    type M[x] = nosi.T[x]
    type A = A0

    def TC = nosi.MTC

    def leibniz = nosi.leibniz
  }

}