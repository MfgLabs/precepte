package com.mfglabs.monitoring

 import scalaz.{Monad}

    trait P[I, A] {
      def map[B](f: A => B): P[I, B] = new P[I, B] {}
      def flatMap[B](f: A => P[I, B]): P[I, B] = new P[I, B] {}
    }

    object P {
      implicit def pInstances[I] =
        new Monad[({type λ[A] = P[I, A]})#λ] {
          override def point[A](a: => A): P[I, A] = new P[I, A] {}

          override def map[A, B](m: P[I, A])(f: A => B): P[I, B] = m.map(f)
          override def bind[A, B](m: P[I, A])(f: A => P[I, B]): P[I, B] = m.flatMap(f)
        }

    }
