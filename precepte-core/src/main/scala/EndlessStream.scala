/*
Copyright 2019 Mfg labs.

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

trait EndlessStream[A] {
  val head: A
  val tail: EndlessStream[A]
}

object EndlessStream {
  def unfold[A, B](init: A)(f: A => (B, A)): EndlessStream[B] =
    new EndlessStream[B] {
      lazy val nextStep: (B, A) = f(init)
      lazy val head: B = nextStep._1
      lazy val tail: EndlessStream[B] = unfold[A, B](nextStep._2)(f)
    }

  /** All items of this EndlessStream are generated via gen */
  def continually[A](gen: => A): EndlessStream[A] =
    new EndlessStream[A] {
      lazy val head: A = gen
      lazy val tail: EndlessStream[A] = continually(gen)
    }
}
