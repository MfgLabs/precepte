package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Monad, Applicative, Functor, \/, \/-, -\/, IndexedStateT, StateT, Semigroup }
import scalaz.syntax.monad._

import scala.annotation.tailrec

private trait ResumeStep[Ta, ManagedState, UnmanagedState, F[_], A, T]
private case class FlatMapStep[Ta, ManagedState, UnmanagedState, F[_], A, T](v: F[(Precepte[Ta, ManagedState, UnmanagedState, F, A], Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, T)]) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A, T]
private case class ReturnStep[Ta, ManagedState, UnmanagedState, F[_], A, T](v: (Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, A, T)) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A, T]

private case class ApplyStep[Ta, ManagedState, UnmanagedState, F[_], A, B, C, T](
  pa: Precepte[Ta, ManagedState, UnmanagedState, F, A],
  pf: Precepte[Ta, ManagedState, UnmanagedState, F, A => B],
  next: B => Precepte[Ta, ManagedState, UnmanagedState, F, C]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, C, T]

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

  @tailrec private final def resume[T](append: (S, T) => T, idx: Int, subG: (S, T) => T)(state: S, t: T)(implicit fu: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState]): ResumeStep[Ta, ManagedState, UnmanagedState, F, A, T] = {
    this match {
        case Return(a) =>
          ReturnStep((state, a, t))

        case Step(st, tags) =>
          val state0 = upd.appendTags(state, tags, idx)
          // append tags to managed state and propagate this new managed state to next step
          FlatMapStep(st.run(state0).map { case (s, p) => (p, s, append(s, t)) })

        case Apply(pa, pf) =>
          ApplyStep(pa, pf, (a: A) => Return(a))

        case f@Flatmap(sub, next) =>
          sub() match {
            case Return(a) =>
              next(a).resume(append, idx, subG)(state, t)

            case Step(st, tags) =>
              val state0 = upd.appendTags(state, tags, idx)
              // repass state as a Step in a Flatmap means the flatMap chain is finished
              // Do not reuse appended segment but original state
              FlatMapStep(st.run(state0).map { case (s, p) =>
                (p.flatMap(next), s, append(s, t))
              })

            case f@Flatmap(sub2, next2) =>
              (Flatmap(sub2, (z: f._I) => next2(z).flatMap(next)):Precepte[Ta, ManagedState, UnmanagedState, F, A]).resume(append, idx, subG)(state, t)

            case Apply(pa, pfa) =>
              ApplyStep(pa, pfa, next)
          }
      }
    }

    final def scan[T](append: (S, T) => T, merge: (T, T) => T, subG: (S, T) => T)(state: S, t: T, idx: Int = 0)(implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[(S, A, T)] = {
      this.resume(append, idx, subG)(state, t) match {
        case FlatMapStep(fsp) =>
          fsp.flatMap { case (p0, s0, t0) => p0.scan(append, merge, subG)(s0, t0) }
        case ReturnStep(sat) =>
          sat.point[F]
        case ApplyStep(pa, pf, next) =>
          (pa.scan(append, merge, subG)(state, t, idx + 1) |@| pf.scan(append, merge, subG)(state, t, idx + 2)).tupled.map {
            case ((s0, a, t0), (s1, f, t1)) =>
              val s = upd.updateUnmanaged(s0, S.append(s0.unmanaged, s1.unmanaged))
              (s, f(a), merge(t0, t1))
          }.flatMap { case (s0, b, t0) =>
            next(b).scan(append, merge, subG)(s0, t0, idx)
          }
      }
    }

    final def run(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[(S, A)] =
      scan[Unit]((_, _) => (), (_, _) => (), (_, _) => ())(state, ()).map{ case (s, a, t) => (s, a) }

    final def eval(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[A] =
      run(state).map(_._2)

    final def observe[T](state: S)(
      implicit
        mo: Monad[F],
        upd: PStateUpdater[Ta, ManagedState, UnmanagedState],
        nod: ToNode[S],
        S: Semigroup[UnmanagedState]
    ): F[(S, A, Graph)] = {
      // graph + leaves
      type G = (Graph, Set[String])

      def append(s: S, g: G): G = {
        val node = nod.toNode(s)
        // val id = s.managed.path.last.tags.callee.value + "_" + s.managed.path.last.id.value
        // val node = (id, s.managed.path.last.tags.callee.value)
        val nodes = g._1.nodes + node
        val es = for { c <- g._2 } yield Edge(c, node.id)
        val edges = g._1.edges ++ es
        Graph(nodes, edges) -> Set(node.id)
      }

      def merge(g0: G, g1: G): G = {
        val nodes = g0._1.nodes ++ g1._1.nodes
        val edges = g0._1.edges ++ g1._1.edges
        val m = edges.map(e => e.from -> e.to).toMap
        val children = nodes.collect {
          case Leaf(id, _) if !m.contains(id) => id
          case Sub(id, _, _) if !m.contains(id) => id
        }
        Graph(nodes, edges) -> children
      }

      def sub(s: S, g: G): G = {
        val node = nod.toNode(s)
        Graph(Set(Sub("cluster_" + node.id, node.value, g._1)), Set.empty) -> Set(node.id)
      }

      scan[G](append _, merge _, sub _)(state, Graph(Set(), Set()) -> Set()).map { case (s, a, g) => (s, a, g._1) }
    }

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

      def applyU[M, U, F[_], A](λ: P[M, U, F, A]#S => F[(U, A)])(implicit F: Functor[F], upd: PStateUpdater[Ta, M, U]): P[M, U, F, A] =
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

  }