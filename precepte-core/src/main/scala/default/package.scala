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
      def appendTags(s: ST[C], tags: BaseTags): PState[BaseTags, MS, C] = {
        import s.managed._
        s.copy(
          managed = s.managed.copy(
            path = path :+ Call(ids.head, tags),
            ids = ids.tail
          ))
      }
      def updateUnmanaged(s: ST[C], unmanaged: C): ST[C] =
        s.copy(unmanaged = unmanaged)

      def spawn(s0: PState[BaseTags, MS, C])
        : (PState[BaseTags, MS, C], PState[BaseTags, MS, C]) = {
        import s0.managed._
        val s1: PState[BaseTags, MS, C] = {
          val idsOdd = ids.evenIndices
          s0.copy(managed = s0.managed.copy(ids = idsOdd))
        }
        val s2: PState[BaseTags, MS, C] = {
          val idsEven = ids.oddIndices
          s0.copy(managed = s0.managed.copy(ids = idsEven))
        }
        (s1, s2)
      }

      /** Recombine the split states s1 and s2 from the [[spawn]] operation
        * back into a single state. s0 is the original state used in [[spawn]].
        *
        * @param s0 the state that was split into (s1',s2').
        *           into its own computation but need to be merged back.
        * @param s1 the state that comes from {{{split(s0)._1}}} and lived its life
        *           into its own computation but need to be merged back.
        * @param s2 the state that comes from {{{split(s0)._2}}} and lived its life
        *           into its own computation but need to be merged back.
        */
      def recombine(s0: S, s1: S, s2: S): S = {
        s0.copy(
          managed = s0.managed.copy(ids = s1.managed.ids.merge(s2.managed.ids)))
      }
    }

  @inline implicit def toNode[C]: ToNode[ST[C]] =
    new ToNode[ST[C]] {
      def toNode(s: ST[C]): Node =
        s.managed.path.lastOption match {
          case Some(last) =>
            val id = last.tags.callee.value + "_" + last.id.name
            Leaf(id, last.tags.callee.value)
          case _ =>
            Leaf("ε", "ε")
        }
    }
}
