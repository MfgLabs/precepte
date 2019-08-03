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

trait MetaGlobalState[S, F[_]] {
  def get: F[S]
  def set(s: S): F[Unit]
}

trait Trampolined[F[_]] extends MetaApplicative[F] {
  def defer[A](ga: => F[A]): F[A]
  def delay[A](a: => A): F[A] = defer(pure(a))
}

trait MetaMonadPrecepte[T, M, U, F[_]]
    extends MetaMonad[F]
    with MetaGlobalState[PState[T, M, U], F]
    with Trampolined[F]

trait ~~>[F[_], G[_]] { self =>
  def apply[A](f: F[A]): G[A]

  @inline final def contrMap[H[_]](pre: H ~~> F): H ~~> G =
    new (H ~~> G) {
      def apply[A](f: H[A]): G[A] = self(pre(f))
    }
  @inline final def map[K[_]](post: G ~~> K): F ~~> K =
    new (F ~~> K) {
      def apply[A](f: F[A]): K[A] = post(self(f))
    }
  @inline final def biMap[H[_], K[_]](pre: H ~~> F, post: G ~~> K): H ~~> K =
    new (H ~~> K) {
      def apply[A](f: H[A]): K[A] = post(self(pre(f)))
    }
}

trait <~~>[F[_], G[_]] { self =>
  def to[A](f: F[A]): G[A]
  def from[A](f: G[A]): F[A]

  def reverse: G <~~> F =
    new (G <~~> F) {
      def to[A](f: G[A]): F[A] = self.from(f)
      def from[A](f: F[A]): G[A] = self.to(f)
      final override def reverse: F <~~> G = self
    }

  @inline final def toK: F ~~> G =
    new (F ~~> G) {
      def apply[A](f: F[A]): G[A] = self.to(f)
    }

  @inline final def fromK: G ~~> F =
    new (G ~~> F) {
      def apply[A](f: G[A]): F[A] = self.from(f)
    }
}
