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

object SubStepInstrumentation {

  /** Change the base functor F to G
    *
    * @tparam T Precepte Tags
    * @tparam M Precepte Managed Stage
    * @tparam U Precepte Unmanaged State
    */
  @inline def iso[T, M, U, F[_], G[_]](iso: F <~~> G)(
      instr: SubStepInstumentation[T, M, U, F])
    : SubStepInstumentation[T, M, U, G] =
    new (SubStepInstumentation[T, M, U, G]) {
      def apply[A](f: Precepte[T, M, U, G, A]): Precepte[T, M, U, G, A] =
        instr(f.compile(iso.reverse)).compile(iso)
    }

  /** Change the type of the Unmanaged State
    *
    * @tparam T Precepte Tags
    * @tparam M Precepte Managed Stage
    * @tparam U Precepte Unmanaged State Type to replace
    * @tparam U2 Precepte Unmanaged State Type to use
    */
  @inline def xmapUnmanagedState[T, M, U, U2, F[_]](to: U => U2, from: U2 => U)(
      instr: SubStepInstumentation[T, M, U, F])
    : SubStepInstumentation[T, M, U2, F] =
    new (SubStepInstumentation[T, M, U2, F]) {
      def apply[A](f: Precepte[T, M, U2, F, A]): Precepte[T, M, U2, F, A] =
        instr(f.xmapState(from, to)).xmapState(to, from)
    }
}
