package com.mfglabs
package precepte

import scala.language.higherKinds
import quiver._
import scalaz.{Monad, Semigroup}
import scalaz.syntax.monad._


package object default {

  type MS = ManagedState[BaseEnv, BaseTags]

  type ST[C] = PState[BaseTags, MS, C]

  type Pre[F[_], C, A] = Precepte[BaseTags, MS, C, F, A]

  object ST {
    def apply[C](span: Span, env: BaseEnv, path: Call.Path[BaseTags], value: C): ST[C] =
      PState[BaseTags, MS, C] (
        ManagedState(env, span, path),
        value
      )
  }

  implicit def pstateUpdater[C] = new PStateUpdater[BaseTags, MS, C] {
    def appendTags(s: ST[C], tags: BaseTags, idx: Int) = {
      val (id, next) = s.managed.ids.run()
      val newId = PId(s"${id.value}_${idx}")
      val is0 = ManagedState(s.managed.env, s.managed.span, s.managed.path :+ Call(newId, tags), next)
      s.copy(managed = is0)
    }
    def updateUnmanaged(s: ST[C], unmanaged: C): ST[C] = s.copy(unmanaged = unmanaged)
  }

}
