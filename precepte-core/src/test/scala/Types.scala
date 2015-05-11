package com.mfglabs.monitoring

 import scalaz.{Monad}

    trait P[F, A] {
      def map[B](f: A => B): P[F, B] = new P[F, B] {}
      def flatMap[B](f: A => P[F, B]): P[F, B] = new P[F, B] {}
    }

    object P {
      implicit def pInstances[F] =
        new Monad[({type λ[A] = P[F, A]})#λ] {
          override def point[A](a: => A): P[F, A] = new P[F, A] {}

          override def map[A, B](m: P[F, A])(f: A => B): P[F, B] = m.map(f)
          override def bind[A, B](m: P[F, A])(f: A => P[F, B]): P[F, B] = m.flatMap(f)
        }

    }
