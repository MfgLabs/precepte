package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Monad, Applicative, Functor, \/, \/-, -\/, IndexedStateT, StateT, Semigroup }
import scalaz.syntax.monad._

import scala.annotation.tailrec

private trait ResumeStep[Ta, ManagedState, UnmanagedState, F[_], A, T]
private case class FlatMapStep[Ta, ManagedState, UnmanagedState, F[_], A, T](v: F[(Precepte[Ta, ManagedState, UnmanagedState, F, A], Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, T)]) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A, T]
private case class ReturnStep[Ta, ManagedState, UnmanagedState, F[_], A, T](v: (Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, A, T)) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A, T]

private case class ApplyStep[Ta, ManagedState, UnmanagedState, F[_], A, B, T](
  pa: Precepte[Ta, ManagedState, UnmanagedState, F, A],
  pf: Precepte[Ta, ManagedState, UnmanagedState, F, A => B]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, B, T]

sealed trait Precepte[Ta, ManagedState, UnmanagedState, F[_], A] {
  self =>

  type PX[A0] = Precepte[Ta, ManagedState, UnmanagedState, F, A0]

  type S = PState[Ta, ManagedState, UnmanagedState]

  type StepState[A0] = StateT[F, S, Precepte[Ta, ManagedState, UnmanagedState, F, A0]]

  final def flatMap[B](f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
    Flatmap[Ta, ManagedState, UnmanagedState, F, A, B](() => self, f)

  final def map[B](f: A => B): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
    flatMap(a => Return(f(a)))

  def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Precepte[Ta, ManagedState, UnmanagedState, F, AP[A]] =
    this.map(a => ap.point(a))

  @tailrec private final def resume[T](node: S => T, append: (Vector[T], T) => T, idx: Int)(state: S, t: T)(implicit fu: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState]): ResumeStep[Ta, ManagedState, UnmanagedState, F, A, T] = {
    def appendNode(s: S, t: T): T = append(Vector(node(s)), t)
    this match {
        case Return(a) =>
          ReturnStep((state, a, t))

        case Step(st, tags) =>
          val state0 = upd.appendTags(state, tags, idx)
          // append tags to managed state and propagate this new managed state to next step
          FlatMapStep(st.run(state0).map { case (s, p) => (p, s, appendNode(s, t)) })

        case Apply(pa, pf) =>
          ApplyStep(pa, pf)

        case f@Flatmap(sub, next) =>
          sub() match {
            case Return(a) =>
              next(a).resume(node, append, idx)(state, t)

            case Step(st, tags) =>
              val state0 = upd.appendTags(state, tags, idx)
              // repass state as a Step in a Flatmap means the flatMap chain is finished
              // Do not reuse appended segment but original state
              FlatMapStep(st.run(state0).map { case (s, p) =>
                (p.flatMap(next), s, appendNode(s, t))
              })

            case f@Flatmap(sub2, next2) =>
              (Flatmap(sub2, (z: f._I) => next2(z).flatMap(next)):Precepte[Ta, ManagedState, UnmanagedState, F, A]).resume(node, append, idx)(state, t)

            case ap@Apply(pa, pfa) =>
              val pfa2 = pfa.map { f => (a: ap._A) => next(f(a)) }
              Apply(pa, pfa2).flatMap(identity).resume(node, append, idx)(state, t)
          }
      }
    }

    final def scan[T](node: S => T, append: (Vector[T], T) => T)(state: S, t: T, idx: Int = 0)(implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[(S, A, T)] = {
      this.resume(node, append, idx)(state, t) match {
        case FlatMapStep(fsp) =>
          fsp.flatMap { case (p0, s0, t0) => p0.scan(node, append)(s0, t0) }
        case ReturnStep(sat) =>
          sat.point[F]
        case ApplyStep(pa, pf) =>
          (pa.scan(node, append)(state, t, idx + 1) |@| pf.scan(node, append)(state, t, idx + 2)).tupled.map {
            case ((s0, a, t0), (s1, f, t1)) =>
              val s = upd.updateUnmanaged(s0, S.append(s0.unmanaged, s1.unmanaged))
              (s, f(a), append(Vector(t0, t1), t))
          }
      }
    }

    final def run(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[(S, A)] =
      scan[Unit](_ => (), (_, _) => ())(state, ()).map{ case (s, a, t) => (s, a) }

    final def eval(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[A] =
      run(state).map(_._2)
  }


case class Return[Ta, ManagedState, UnmanagedState, F[_], A](a: A) extends Precepte[Ta, ManagedState, UnmanagedState, F, A]

case class Step[Ta, ManagedState, UnmanagedState, F[_], A](
  st: Precepte[Ta, ManagedState, UnmanagedState, F, A]#StepState[A],
  tags: Ta
) extends Precepte[Ta, ManagedState, UnmanagedState, F, A]

case class Flatmap[Ta, ManagedState, UnmanagedState, F[_], I, A](
  sub: () => Precepte[Ta, ManagedState, UnmanagedState, F, I],
  next: I => Precepte[Ta, ManagedState, UnmanagedState, F, A]
) extends Precepte[Ta, ManagedState, UnmanagedState, F, A] {
  type _I = I
}

case class Apply[Ta, ManagedState, UnmanagedState, F[_], A, B](
  pa: Precepte[Ta, ManagedState, UnmanagedState, F, A],
  pf: Precepte[Ta, ManagedState, UnmanagedState, F, A => B]
) extends Precepte[Ta, ManagedState, UnmanagedState, F, B] {
  type _A = A
}

trait LowPriorityManagedStatetances {
  implicit def precepteMonadManagedStatetance[Ta, ManagedState, UnmanagedState, F[_]] =
    new Monad[({ type λ[α] = Precepte[Ta, ManagedState, UnmanagedState, F, α] })#λ] {
      override def point[A](a: => A): Precepte[Ta, ManagedState, UnmanagedState, F, A] =
        Return(a)
      override def map[A, B](m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(f: A => B): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.map(f)
      override def bind[A, B](m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.flatMap(f)

      override def ap[A, B](pa: => Precepte[Ta, ManagedState, UnmanagedState, F, A])(pab: => Precepte[Ta, ManagedState, UnmanagedState, F, A => B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        Apply(pa, pab)
    }

}

  object Precepte extends LowPriorityManagedStatetances {

    trait PrecepteBuilder[Ta] {
      val tags: Ta

      type P[ManagedState, UnmanagedState, F[_], A] = Precepte[Ta, ManagedState, UnmanagedState, F, A]

      import scala.concurrent.Future
      def future[M, U, A](λ: P[M, U, Future, A]#S => Future[A])(implicit F: Functor[Future], ec: scala.concurrent.ExecutionContext): P[M, U, Future, Throwable \/ A] =
        apply { pa =>
          λ(pa).map(\/-.apply _)
            .recover{ case e => -\/(e) }
        }

      def apply[M, U, F[_], A](λ: P[M, U, F, A]#S => F[A])(implicit F: Functor[F]): P[M, U, F, A] =
        Step[Ta, M, U, F, A](
          IndexedStateT { (st: P[M, U, F, A]#S) =>
            for (a <- λ(st))
            yield st -> Return(a)
          }, tags)

      def applyS[M, U, F[_], A](λ: P[M, U, F, A]#S => F[(U, A)])(implicit F: Functor[F], upd: PStateUpdater[Ta, M, U]): P[M, U, F, A] =
        Step[Ta, M, U, F, A](
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

    def apply[Ta](_tags: Ta) =
      new PrecepteBuilder[Ta] {
        val tags = _tags
      }

    trait *->*[F0[_]] {}
    trait *->*->*[F0[_, _]] {}

    implicit def fKindEv[F0[_]] = new *->*[F0] {}
    implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}

    def trans[Ta, ManagedState, UnmanagedState, F[_], G[_]: *->*, A](
      m: Precepte[Ta, ManagedState, UnmanagedState, F, G[A]]
    )(implicit hh: HasHoist[G]): hh.T[({ type λ[α] = Precepte[Ta, ManagedState, UnmanagedState, F, α] })#λ, A] =
      hh.lift[({ type λ[α] = Precepte[Ta, ManagedState, UnmanagedState, F, α] })#λ, A](m)

    def trans[Ta, ManagedState, UnmanagedState, F[_], G[_, _]: *->*->*, A, B](
      m: Precepte[Ta, ManagedState, UnmanagedState, F, G[A, B]]
    )(implicit hh: HasHoist[({ type λ[α] = G[A, α] })#λ]): hh.T[({ type λ[α] = Precepte[Ta, ManagedState, UnmanagedState, F, α] })#λ, B] = {
      type λ[α] = G[A, α]
      trans[Ta, ManagedState, UnmanagedState, F, λ, B](m)(new *->*[λ] {}, hh)
    }

  }