package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Bind, Monad, MonadPlus, Applicative, Functor, \/, \/-, -\/, IndexedStateT, StateT, State, Traverse, ~> }
import scalaz.Isomorphism.{<~>, <=>, isoRefl, isoNaturalRefl}
import scalaz.syntax.monad._

import scala.annotation.tailrec
import scalaz.{TreeLoc, Tree}


trait ResumeStep[Tags, ManagedState, UnmanagedState, F[_], A, T]
case class FlatMapStep[Tags, ManagedState, UnmanagedState, F[_], A, T](v: F[(Pre[Tags, ManagedState, UnmanagedState, F, A], Pre[Tags, ManagedState, UnmanagedState, F, A]#S, T)]) extends ResumeStep[Tags, ManagedState, UnmanagedState, F, A, T]
case class ReturnStep[Tags, ManagedState, UnmanagedState, F[_], A, T](v: (Pre[Tags, ManagedState, UnmanagedState, F, A]#S, A, T)) extends ResumeStep[Tags, ManagedState, UnmanagedState, F, A, T]

sealed trait Pre[Tags, ManagedState, UnmanagedState, F[_], A] {
  self =>

  type PX[A] = Pre[Tags, ManagedState, UnmanagedState, F, A]

  type S = PState[Tags, ManagedState, UnmanagedState]

  type StepState[A] = StateT[F, S, Pre[Tags, ManagedState, UnmanagedState, F, A]]

  final def flatMap[B](f: A => Pre[Tags, ManagedState, UnmanagedState, F, B]): Pre[Tags, ManagedState, UnmanagedState, F, B] =
    Flatmap[Tags, ManagedState, UnmanagedState, F, A, B](() => self, f)

  final def map[B](f: A => B): Pre[Tags, ManagedState, UnmanagedState, F, B] =
    flatMap(a => Return(f(a)))

  def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Pre[Tags, ManagedState, UnmanagedState, F, AP[A]] =
    this.map(a => ap.point(a))

  @tailrec private final def resume[T](state: S, t: T, z: (S, T) => T)(implicit fu: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): ResumeStep[Tags, ManagedState, UnmanagedState, F, A, T] = this match {
      case Return(a) =>
        ReturnStep((state, a, t))

      case Step(st, tags) =>
        val state0 = upd.appendTags(state, tags)
        // append tags to managed state and propagate this new managed state to next step
        FlatMapStep(st.run(state0).map { case (s, p) => (p, s, z(s, t)) })

      case Flatmap(sub, next) =>
        sub() match {
          case Return(a) =>
            next(a).resume(state, t, z)

          case Step(st, tags) =>
            val state0 = upd.appendTags(state, tags)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            // Do not reuse appended segment but original state
            FlatMapStep(st.run(state0).map { case (s, p) =>
              // val s1 = upd.updateUnmanaged(state, s.unmanaged)
              (p.flatMap(next), s, z(s, t))
            })

          case f@Flatmap(sub2, next2) =>
            (Flatmap(sub2, (z:f._I) => next2(z).flatMap(next)):Pre[Tags, ManagedState, UnmanagedState, F, A]).resume(state, t, z)

        }
    }

  final def scan[T](state: S, t: T, z: (S, T) => T)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[(S, A, T)] =
    this.resume(state, t, z) match {
      case FlatMapStep(fsp) =>
        fsp.flatMap { case (p0, s0, t0) => p0.scan(s0, t0, z) }
      case ReturnStep(sat) =>
        sat.point[F]
    }

  final def run(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[(S, A)] =
    scan[Unit](state, (), (_, _) => ()).map{ case (s, a, t) => (s, a) }

  final def eval(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[A] =
    run(state).map(_._2)

  final def observe(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[(S, A, Vector[S])] =
    scan(state, Vector.empty[S], (s, t) => t :+ s)

}


case class Return[Tags, ManagedState, UnmanagedState, F[_], A](a: A) extends Pre[Tags, ManagedState, UnmanagedState, F, A]

case class Step[Tags, ManagedState, UnmanagedState, F[_], A](
  st: Pre[Tags, ManagedState, UnmanagedState, F, A]#StepState[A],
  tags: Tags
) extends Pre[Tags, ManagedState, UnmanagedState, F, A]

case class Flatmap[Tags, ManagedState, UnmanagedState, F[_], I, A](
  sub: () => Pre[Tags, ManagedState, UnmanagedState, F, I],
  next: I => Pre[Tags, ManagedState, UnmanagedState, F, A]
) extends Pre[Tags, ManagedState, UnmanagedState, F, A] {
  type _I = I
}



trait LowPriorityManagedStatetances {
  implicit def precepteMonadManagedStatetance[Tags, ManagedState, UnmanagedState, F[_]](implicit ApF: Applicative[F]) =
    new Monad[({ type λ[α] = Pre[Tags, ManagedState, UnmanagedState, F, α] })#λ] {
      override def point[A](a: => A): Pre[Tags, ManagedState, UnmanagedState, F, A] =
        Return(a)
      override def map[A, B](m: Pre[Tags, ManagedState, UnmanagedState, F, A])(f: A => B): Pre[Tags, ManagedState, UnmanagedState, F, B] =
        m.map(f)
      override def bind[A, B](m: Pre[Tags, ManagedState, UnmanagedState, F, A])(f: A => Pre[Tags, ManagedState, UnmanagedState, F, B]): Pre[Tags, ManagedState, UnmanagedState, F, B] =
        m.flatMap(f)

      // override to support parallel execution
      // override def ap[A, B](pa: => Pre[Tags, ManagedState, UnmanagedState, F, A])(pab: => Pre[Tags, ManagedState, UnmanagedState, F, A => B]) =
      //   pa.flatMapK { fa =>
      //     pab.mapK { fab =>
      //       fa <*> fab.map { case (s, ab) => (s: (S, A)) => ab(s._2) }
      //     }
      //   }
    }

}

  object Pre extends LowPriorityManagedStatetances {

    trait PrecepteBuilder[Tags, ManagedState, UnmanagedState, F[_]] {
      val tags: Tags

      type P[A] = Pre[Tags, ManagedState, UnmanagedState, F, A]

      def apply[A](λ: P[A]#S => F[A])(implicit F: Functor[F]): P[A] =
        Step[Tags, ManagedState, UnmanagedState, F, A](
          IndexedStateT { (st: P[A]#S) =>
            for (a <- λ(st))
            yield st -> Return(a)
          }, tags)

      def applyS[A](λ: P[A]#S => F[(UnmanagedState, A)])(implicit F: Functor[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): P[A] =
        Step[Tags, ManagedState, UnmanagedState, F, A](
          IndexedStateT { (st: P[A]#S) =>
            for (ca <- λ(st))
            yield {
              val (unmanaged, a) = ca
              upd.updateUnmanaged(st, unmanaged) -> Return(a)
            }
          }, tags)

      def apply[A](m: P[A])(implicit A: Applicative[F]): P[A] =
        Step(IndexedStateT[F, P[A]#S, P[A]#S, P[A]]{ st =>
          (st -> m).point[F]
        }, tags)
    }

    def apply[Tags, ManagedState, UnmanagedState, F[_]](_tags: Tags) =
      new PrecepteBuilder[Tags, ManagedState, UnmanagedState, F] {
        val tags = _tags
      }

    trait *->*[F0[_]] {}
    trait *->*->*[F0[_, _]] {}

    implicit def fKindEv[F0[_]] = new *->*[F0] {}
    implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}

    def trans[Tags, ManagedState, UnmanagedState, F[_], G[_]: *->*, A](
      m: Pre[Tags, ManagedState, UnmanagedState, F, G[A]]
    )(implicit hh: HasHoist[G]): hh.T[({ type λ[α] = Pre[Tags, ManagedState, UnmanagedState, F, α] })#λ, A] =
      hh.lift[({ type λ[α] = Pre[Tags, ManagedState, UnmanagedState, F, α] })#λ, A](m)

    def trans[Tags, ManagedState, UnmanagedState, F[_], G[_, _]: *->*->*, A, B](
      m: Pre[Tags, ManagedState, UnmanagedState, F, G[A, B]]
    )(implicit hh: HasHoist[({ type λ[α] = G[A, α] })#λ]): hh.T[({ type λ[α] = Pre[Tags, ManagedState, UnmanagedState, F, α] })#λ, B] = {
      type λ[α] = G[A, α]
      trans[Tags, ManagedState, UnmanagedState, F, λ, B](m)(new *->*[λ] {}, hh)
    }
  }