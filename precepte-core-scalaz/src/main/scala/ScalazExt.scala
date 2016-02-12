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
package corescalaz

import scalaz.~>
import scalaz.Isomorphism.<~>

import scala.language.higherKinds


/** Implicit conversions to correct these functions doesn't work, scalac still uses original ones */
case class ScalazExt[Ta, ManagedState, UnmanagedState, F[_], A](
  p: Precepte[Ta, ManagedState, UnmanagedState, F, A]
) extends AnyVal {
  final def compile[G[_]](iso: F <~> G): Precepte[Ta, ManagedState, UnmanagedState, G, A] = p.compile(ScalazMetaIso(iso))

  final def mapSuspension(nat: p.SF ~> F): Precepte[Ta, ManagedState, UnmanagedState, F, A] = p.mapSuspension(ScalazMetaNat(nat))
} 