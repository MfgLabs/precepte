package com.mfglabs

import scala.language.higherKinds


package object precepte {

  case class ManagedState0[T <: Tags](path: Call.Path[T], ids: PIdStream = PIdStream())
  case class FreeState0[E <: Env, T <: Tags, C](span: Span, env: E, value: C)

  type PIS0 = ManagedState0[BaseTags]
  type PES0[C] = FreeState0[BaseEnv, BaseTags, C]
  type PST0[C] = PState0[BaseTags, PIS0, PES0[C]]

  object PST0 {
    def apply[C](span: Span, env: BaseEnv, path: Call.Path[BaseTags], value: C): PST0[C] =
      PState0[BaseTags, PIS0, PES0[C]] (
        ManagedState0(path),
        FreeState0(span, env, value)
      )
  }

  type PCTX0[F[_], C] = TaggingContext[BaseTags, PIS0, PES0[C], PST0[C], F]

  implicit def updater0[C] = new PStateUpdater[BaseTags, PIS0, PES0[C], PST0[C]] {
    def appendTags(s: PST0[C], tags: BaseTags) = {
      val (id, next) = s.managed.ids.run()
      val is0 = ManagedState0(s.managed.path :+ Call(id, tags), next)
      s.copy(managed = is0)
    }
    def updateFree(s: PST0[C], ext: PES0[C]): PST0[C] = s.copy(free = ext)
  }
}