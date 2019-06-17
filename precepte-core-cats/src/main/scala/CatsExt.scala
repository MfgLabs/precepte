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
package corecats

import cats.~>

import scala.language.higherKinds

/** Implicit conversions to correct these functions doesn't work, scalac still uses original ones */
final class CatsExt[Ta, ManagedState, UnmanagedState, F[_], A](
    val p: Precepte[Ta, ManagedState, UnmanagedState, F, A]
) extends AnyVal {
  final def compile[G[_]](
      to: F ~> G,
      from: G ~> F): Precepte[Ta, ManagedState, UnmanagedState, G, A] =
    p.compile(CatsMetaIso(to, from))

  final def mapSuspension(
      nat: λ[α => (PState[Ta, ManagedState, UnmanagedState], F[α])] ~> F)
    : Precepte[Ta, ManagedState, UnmanagedState, F, A] =
    p.mapSuspension(
      CatsMetaNat[λ[α => (PState[Ta, ManagedState, UnmanagedState], F[α])], F](
        nat))
}
