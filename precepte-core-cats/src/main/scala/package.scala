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

import cats.data.{ OptionT, XorT, Xor, StreamingT }
import scala.language.higherKinds


package object corecats {

  implicit object optionHasHoist extends HasHoist[Option] {
    type T[F[_], A] = OptionT[F, A]
    def lift[F[_], A](f: F[Option[A]]): OptionT[F, A] = OptionT(f)
  }

  // implicit object streamingTHasHoist extends HasHoist[StreamingT] {
  //   type T[F[_], A] = StreamingT[F, A]
  //   def lift[F[_], A](f: F[StreamingT[A]]): StreamingT[F, A] = StreamingT.wait(f)
  // }

  implicit def xorHasHoist[A]: HasHoist.Aux[({ type λ[α] = Xor[A, α] })#λ, ({ type λ[F[_], B] = XorT[F, A, B] })#λ] = new XorHasHoist[A]
}
