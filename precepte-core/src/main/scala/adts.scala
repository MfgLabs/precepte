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


/** A typed group of tags */
trait Tags
object NoTags extends Tags

/** The state gathering all data concerning current execution context
  * 
  * @param managed the state containing the context from where it comes & where it is and managed by Precepte
  * @param unmanaged the container where you can put anything you want to manage your state 
  */
case class PState[Ta, ManagedState, UnmanagedState](
  managed: ManagedState,
  unmanaged: UnmanagedState)

/** A Typeclass representing an updatable Precepte state */
trait PStateUpdater[Ta, MS, FS] {
  type S = PState[Ta, MS, FS]
  def appendTags(s: S, t: Ta, idx: Int): S
  def updateUnmanaged(s: S, ext: FS): S
}
