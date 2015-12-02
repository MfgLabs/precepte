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

// import scalaz.{ OptionT, ListT, EitherT, \/ }

trait HasHoist[M[_]] {
  type T[_[_], _]
  def lift[F[_], A](f: F[M[A]]): T[F, A]
}

object HasHoist {
  type Aux[M[_], T0[_[_], _]] = HasHoist[M] { type T[F[_], A] = T0[F, A] }

  def apply[M[_]](implicit h: HasHoist[M]): Aux[M, h.T] = h

  // implicit object optionHasHoist extends HasHoist[Option] {
  //   type T[F[_], A] = OptionT[F, A]
  //   def lift[F[_], A](f: F[Option[A]]): OptionT[F, A] = OptionT.optionT[F](f)
  // }

  // implicit object listHasHoist extends HasHoist[List] {
  //   type T[F[_], A] = ListT[F, A]
  //   def lift[F[_], A](f: F[List[A]]): ListT[F, A] = ListT.apply(f)
  // }

  // private[this] class EitherHasHoist[A] extends HasHoist[({ type λ[α] = A \/ α })#λ] {
  //   type T[F[_], B] = EitherT[F, A, B]
  //   def lift[F[_], B](f: F[A \/ B]): EitherT[F, A, B] = EitherT.apply(f)
  // }

  // implicit def eitherHasHoist[A]: HasHoist.Aux[({ type λ[α] = A \/ α })#λ, ({ type λ[F[_], B] = EitherT[F, A, B] })#λ] = new EitherHasHoist[A]
}