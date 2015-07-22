package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Monad, Applicative, Functor, \/, \/-, -\/, IndexedStateT, StateT, Semigroup }
import scalaz.syntax.monad._

import scala.annotation.tailrec

private trait ResumeStep[Tags, ManagedState, UnmanagedState, F[_], A, T]
private case class FlatMapStep[Tags, ManagedState, UnmanagedState, F[_], A, T](v: F[(Precepte[Tags, ManagedState, UnmanagedState, F, A], Precepte[Tags, ManagedState, UnmanagedState, F, A]#S, T)]) extends ResumeStep[Tags, ManagedState, UnmanagedState, F, A, T]
private case class ReturnStep[Tags, ManagedState, UnmanagedState, F[_], A, T](v: (Precepte[Tags, ManagedState, UnmanagedState, F, A]#S, A, T)) extends ResumeStep[Tags, ManagedState, UnmanagedState, F, A, T]

private case class ApplyStep[Tags, ManagedState, UnmanagedState, F[_], A, B, T](
  pa: Precepte[Tags, ManagedState, UnmanagedState, F, A],
  pf: Precepte[Tags, ManagedState, UnmanagedState, F, A => B]
) extends ResumeStep[Tags, ManagedState, UnmanagedState, F, B, T]

sealed trait Precepte[Tags, ManagedState, UnmanagedState, F[_], A] {
  self =>

  type PX[A] = Precepte[Tags, ManagedState, UnmanagedState, F, A]

  type S = PState[Tags, ManagedState, UnmanagedState]

  type StepState[A] = StateT[F, S, Precepte[Tags, ManagedState, UnmanagedState, F, A]]

  final def flatMap[B](f: A => Precepte[Tags, ManagedState, UnmanagedState, F, B]): Precepte[Tags, ManagedState, UnmanagedState, F, B] =
    Flatmap[Tags, ManagedState, UnmanagedState, F, A, B](() => self, f)

  final def map[B](f: A => B): Precepte[Tags, ManagedState, UnmanagedState, F, B] =
    flatMap(a => Return(f(a)))

  def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Precepte[Tags, ManagedState, UnmanagedState, F, AP[A]] =
    this.map(a => ap.point(a))

  @tailrec private final def resume[T](state: S, t: T, z: (S, T) => T)(implicit fu: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): ResumeStep[Tags, ManagedState, UnmanagedState, F, A, T] = this match {
      case Return(a) =>
        ReturnStep((state, a, t))

      case Step(st, tags) =>
        val state0 = upd.appendTags(state, tags)
        // append tags to managed state and propagate this new managed state to next step
        FlatMapStep(st.run(state0).map { case (s, p) => (p, s, z(s, t)) })

      case Apply(pa, pf) =>
        ApplyStep(pa, pf)

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
            (Flatmap(sub2, (z:f._I) => next2(z).flatMap(next)):Precepte[Tags, ManagedState, UnmanagedState, F, A]).resume(state, t, z)

          case ap@Apply(pa, pfa) =>
            val pfa2 = pfa.map { f => (a: ap._A) => next(f(a)) }
            Apply(pa, pfa2).flatMap(identity).resume(state, t, z)
        }
    }

  final def scan[T](state: S, t: T, z: (S, T) => T)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[(S, A, T)] =
    this.resume(state, t, z) match {
      case FlatMapStep(fsp) =>
        fsp.flatMap { case (p0, s0, t0) => p0.scan(s0, t0, z) }
      case ReturnStep(sat) =>
        sat.point[F]
      case ApplyStep(pa, pf) =>
        (pa.scan(state, t, z) |@| pf.scan(state, t, z)).tupled.map {
          case ((s0, a, t0), (s1, f, t1)) =>
            val s = upd.updateUnmanaged(s0, S.append(s0.unmanaged, s1.unmanaged))
            (s, f(a), t0)
        }
    }

  final def run(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[(S, A)] =
    scan[Unit](state, (), (_, _) => ()).map{ case (s, a, t) => (s, a) }

  final def eval(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[A] =
    run(state).map(_._2)

  final def observe(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[(S, A, Vector[S])] =
    scan(state, Vector.empty[S], (s, t) => t :+ s)

}


case class Return[Tags, ManagedState, UnmanagedState, F[_], A](a: A) extends Precepte[Tags, ManagedState, UnmanagedState, F, A]

case class Step[Tags, ManagedState, UnmanagedState, F[_], A](
  st: Precepte[Tags, ManagedState, UnmanagedState, F, A]#StepState[A],
  tags: Tags
) extends Precepte[Tags, ManagedState, UnmanagedState, F, A]

case class Flatmap[Tags, ManagedState, UnmanagedState, F[_], I, A](
  sub: () => Precepte[Tags, ManagedState, UnmanagedState, F, I],
  next: I => Precepte[Tags, ManagedState, UnmanagedState, F, A]
) extends Precepte[Tags, ManagedState, UnmanagedState, F, A] {
  type _I = I
}

case class Apply[Tags, ManagedState, UnmanagedState, F[_], A, B](
  pa: Precepte[Tags, ManagedState, UnmanagedState, F, A],
  pf: Precepte[Tags, ManagedState, UnmanagedState, F, A => B]
) extends Precepte[Tags, ManagedState, UnmanagedState, F, B] {
  type _A = A
}

trait LowPriorityManagedStatetances {
  implicit def precepteMonadManagedStatetance[Tags, ManagedState, UnmanagedState, F[_]] =
    new Monad[({ type λ[α] = Precepte[Tags, ManagedState, UnmanagedState, F, α] })#λ] {
      override def point[A](a: => A): Precepte[Tags, ManagedState, UnmanagedState, F, A] =
        Return(a)
      override def map[A, B](m: Precepte[Tags, ManagedState, UnmanagedState, F, A])(f: A => B): Precepte[Tags, ManagedState, UnmanagedState, F, B] =
        m.map(f)
      override def bind[A, B](m: Precepte[Tags, ManagedState, UnmanagedState, F, A])(f: A => Precepte[Tags, ManagedState, UnmanagedState, F, B]): Precepte[Tags, ManagedState, UnmanagedState, F, B] =
        m.flatMap(f)

      override def ap[A, B](pa: => Precepte[Tags, ManagedState, UnmanagedState, F, A])(pab: => Precepte[Tags, ManagedState, UnmanagedState, F, A => B]): Precepte[Tags, ManagedState, UnmanagedState, F, B] =
        Apply(pa, pab)
    }

}

  object Precepte extends LowPriorityManagedStatetances {

    trait PrecepteBuilder[Tags] {
      val tags: Tags

      type P[ManagedState, UnmanagedState, F[_], A] = Precepte[Tags, ManagedState, UnmanagedState, F, A]

      import scala.concurrent.Future
      def future[M, U, A](λ: P[M, U, Future, A]#S => Future[A])(implicit F: Functor[Future], ec: scala.concurrent.ExecutionContext): P[M, U, Future, Throwable \/ A] =
        apply { pa =>
          λ(pa).map(\/-.apply _)
            .recover{ case e => -\/(e) }
        }

      def apply[M, U, F[_], A](λ: P[M, U, F, A]#S => F[A])(implicit F: Functor[F]): P[M, U, F, A] =
        Step[Tags, M, U, F, A](
          IndexedStateT { (st: P[M, U, F, A]#S) =>
            for (a <- λ(st))
            yield st -> Return(a)
          }, tags)

      def applyS[M, U, F[_], A](λ: P[M, U, F, A]#S => F[(U, A)])(implicit F: Functor[F], upd: PStateUpdater[Tags, M, U]): P[M, U, F, A] =
        Step[Tags, M, U, F, A](
          IndexedStateT { (st: P[M, U, F, A]#S) =>
            for (ca <- λ(st))
            yield {
              val (unmanaged, a) = ca
              upd.updateUnmanaged(st, unmanaged) -> Return(a)
            }
          }, tags)

      def apply[M, U, F[_], A](m: P[M, U, F, A])(implicit A: Applicative[F]): P[M, U, F, A] =
        Step(IndexedStateT[F, P[M, U, F, A]#S, P[M, U, F, A]#S, P[M, U, F, A]]{ st =>
          (st -> m).point[F]
        }, tags)
    }

    def apply[Tags](_tags: Tags) =
      new PrecepteBuilder[Tags] {
        val tags = _tags
      }

    trait *->*[F0[_]] {}
    trait *->*->*[F0[_, _]] {}

    implicit def fKindEv[F0[_]] = new *->*[F0] {}
    implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}

    def trans[Tags, ManagedState, UnmanagedState, F[_], G[_]: *->*, A](
      m: Precepte[Tags, ManagedState, UnmanagedState, F, G[A]]
    )(implicit hh: HasHoist[G]): hh.T[({ type λ[α] = Precepte[Tags, ManagedState, UnmanagedState, F, α] })#λ, A] =
      hh.lift[({ type λ[α] = Precepte[Tags, ManagedState, UnmanagedState, F, α] })#λ, A](m)

    def trans[Tags, ManagedState, UnmanagedState, F[_], G[_, _]: *->*->*, A, B](
      m: Precepte[Tags, ManagedState, UnmanagedState, F, G[A, B]]
    )(implicit hh: HasHoist[({ type λ[α] = G[A, α] })#λ]): hh.T[({ type λ[α] = Precepte[Tags, ManagedState, UnmanagedState, F, α] })#λ, B] = {
      type λ[α] = G[A, α]
      trans[Tags, ManagedState, UnmanagedState, F, λ, B](m)(new *->*[λ] {}, hh)
    }

  }