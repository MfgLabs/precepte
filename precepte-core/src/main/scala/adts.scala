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

/** The state gathering all data concerning current execution context
  *
  * @param managed the state containing the context from where it comes & where it is and managed by Precepte
  * @param unmanaged the container where you can put anything you want to manage your state
  */
final case class PState[Ta, ManagedState, UnmanagedState](
    managed: ManagedState,
    unmanaged: UnmanagedState) {

  def mapUnmanaged[UnmanagedState2](f: UnmanagedState => UnmanagedState2)
    : PState[Ta, ManagedState, UnmanagedState2] = PState(managed, f(unmanaged))
}

/** A Typeclass representing an updatable Precepte state (can append a Tag+idx & update unmanaged part)
  *
  * @tparam Tag Type of Precepte Tag
  * @tparam MS Type of Managed State
  * @tparam US Type of Unmanaged State
  */
trait PStateUpdater[Tag, MS, US] { self =>
  final type S = PState[Tag, MS, US]

  def appendTags(s: S, t: Tag): S
  def updateUnmanaged(s: S, ext: US): S

  /** Split the s0 state into two distinct substates (s1,s2)
    * that can be used in different branches of the computation.
    *
    * Some Precepte operations like [[Precepte.ap]] use parallel
    * computations, so they need to be able to fork the state
    * but should also avoid using what should be unique twice
    * like identifiers.
    *
    * So spawn is used to ensure that s1 and s2 do not share
    * unique values.
    * @param s0
    * @return (s1, s2)
    */
  def spawn(s0: S): (S, S)

  /** Recombine the spawn states s1 and s2 from the [[spawn]] operation
    * back into a single state. s0 is the original state used in [[spawn]].
    *
    * @param s0 the state that was spawn into (s1',s2').
    *           into its own computation but need to be merged back.
    * @param s1 the state that comes from {{{spawn(s0)._1}}} and lived its life
    *           into its own computation but need to be merged back.
    * @param s2 the state that comes from {{{spawn(s0)._2}}} and lived its life
    *           into its own computation but need to be merged back.
    */
  def recombine(s0: S, s1: S, s2: S): S

  @inline final def xmapUnmanaged[US2](
      to: US => US2,
      from: US2 => US): PStateUpdater[Tag, MS, US2] =
    new PStateUpdater[Tag, MS, US2] {
      final type S2 = PState[Tag, MS, US2]

      def appendTags(s: S2, t: Tag): S2 =
        self.appendTags(s.mapUnmanaged(from), t).mapUnmanaged(to)

      def updateUnmanaged(s: S2, ext: US2): S2 =
        self.updateUnmanaged(s.mapUnmanaged(from), from(ext)).mapUnmanaged(to)

      def spawn(s0: S2): (S2, S2) = {
        val (s1, s2) = self.spawn(s0.mapUnmanaged(from))
        (s1.mapUnmanaged(to), s2.mapUnmanaged(to))
      }

      def recombine(s0: S2, s1: S2, s2: S2): S2 = {
        val s0p = s0.mapUnmanaged(from)
        val s1p = s1.mapUnmanaged(from)
        val s2p = s2.mapUnmanaged(from)
        self.recombine(s0p, s1p, s2p).mapUnmanaged(to)
      }
    }
}
