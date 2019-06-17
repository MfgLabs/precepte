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

import scala.language.higherKinds

/**
  * Default implementation of Precepte using:
  *   - BaseTags for Precepte Tags
  *   - ManagedState[BaseEnv, BaseTags] for the managed part.
  *
  * To use it, just do: `import com.mfglabs.precepte._`
  */
package object default extends HK {

  final type DefaultPre[F[_], C, A] = Precepte[BaseTags, MS, C, F, A]
  final type DPre[F[_], C, A] = DefaultPre[F, C, A]

  final type MS = ManagedState[BaseEnv, BaseTags]

  /** A simpler *->* type alias for internal state */
  final type ST[C] = PState[BaseTags, MS, C]

  final object ST {
    def apply[C](span: Span,
                 env: BaseEnv,
                 path: Call.Path[BaseTags],
                 value: C): ST[C] =
      apply[C](
        ManagedState(env, span, path),
        value
      )

    def apply[C](managed: MS, unmanaged: C): ST[C] =
      PState[BaseTags, MS, C](
        managed,
        unmanaged
      )
  }

  // def tags(callee: String) = BaseTags(Callee(callee), Category.Database)

  @inline implicit def pstateUpdater[C]: PStateUpdater[BaseTags, MS, C] =
    new PStateUpdater[BaseTags, MS, C] {
      def appendTags(s: ST[C], tags: BaseTags, idx: Int) = {
        val ids = s.managed.ids
        val newId = PId(s"${ids.head.value}_${idx}")
        val is0 = ManagedState(s.managed.env,
                               s.managed.span,
                               s.managed.path :+ Call(newId, tags),
                               ids.tail)
        s.copy(managed = is0)
      }
      def updateUnmanaged(s: ST[C], unmanaged: C): ST[C] =
        s.copy(unmanaged = unmanaged)
    }

  @inline implicit def toNode[C]: ToNode[ST[C]] =
    new ToNode[ST[C]] {
      def toNode(s: ST[C]): Node =
        s.managed.path.lastOption match {
          case Some(last) =>
            val id = last.tags.callee.value + "_" + last.id.value
            Leaf(id, last.tags.callee.value)
          case _ =>
            Leaf("ε", "ε")
        }
    }

}
