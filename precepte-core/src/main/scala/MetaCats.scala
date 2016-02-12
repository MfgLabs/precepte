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


trait MetaSemigroup[A] {
  def combine(a: A, b: A): A
}

trait MetaFunctor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait MetaApplicative[F[_]] extends MetaFunctor[F] {
  def pure[A](x: A): F[A]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  final def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    ap(fb)(map(fa)(a => (b: B) => f(a, b)))
}

trait MetaMonad[F[_]] extends MetaApplicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}


trait ~~>[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}

trait <~~>[F[_], G[_]] {
  def to[A](f: F[A]): G[A]
  def from[A](f: G[A]): F[A]
}
