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

trait EndlessStream[A] { self =>
  val head: A
  val tail: EndlessStream[A]

  @inline final def evenIndices: EndlessStream[A] =
    new EndlessStream[A] {
      lazy val head: A = self.head
      lazy val tail: EndlessStream[A] = self.tail.tail.evenIndices
    }

  final def oddIndices: EndlessStream[A] =
    EndlessStream.defer(self.tail.evenIndices)

  final def merge(other: EndlessStream[A]): EndlessStream[A] =
    new EndlessStream[A] {
      lazy val head = self.head
      val tail = new EndlessStream[A] {
        lazy val head: A = other.head
        lazy val tail: EndlessStream[A] = self.tail.merge(other.tail)
      }
    }

  @inline final def take(n: Int): List[A] =
    if (n <= 0) Nil
    else self.head :: self.tail.take(n - 1)
}

object EndlessStream extends MetaDefer[EndlessStream] {
  @inline final def unfold[A, B](init: A)(f: A => (B, A)): EndlessStream[B] =
    new EndlessStream[B] {
      lazy val nextStep: (B, A) = f(init)
      lazy val head: B = nextStep._1
      lazy val tail: EndlessStream[B] = unfold[A, B](nextStep._2)(f)
    }

  /** All items of this EndlessStream are generated via gen */
  @inline final def continually[A](gen: => A): EndlessStream[A] =
    new EndlessStream[A] {
      lazy val head: A = gen
      lazy val tail: EndlessStream[A] = continually(gen)
    }

  @inline final def defer[A](ga: => EndlessStream[A]): EndlessStream[A] =
    new EndlessStream[A] {
      lazy val defered: EndlessStream[A] = ga
      lazy val head: A = defered.head
      lazy val tail: EndlessStream[A] = defered.tail
    }
}
