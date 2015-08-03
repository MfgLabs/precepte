package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Monad, Applicative, Functor, \/, \/-, -\/, IndexedStateT, StateT, Semigroup }
import scalaz.syntax.monad._

import scala.annotation.tailrec

private sealed trait ResumeStep[Ta, ManagedState, UnmanagedState, F[_], A]

private case class SimpleStep[Ta, ManagedState, UnmanagedState, F[_], A](
  v: F[(Precepte[Ta, ManagedState, UnmanagedState, F, A], Precepte[Ta, ManagedState, UnmanagedState, F, A]#S)]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A]

private case class FlatMapStep[Ta, ManagedState, UnmanagedState, F[_], I, A](
  v: F[(Precepte[Ta, ManagedState, UnmanagedState, F, I], I => Precepte[Ta, ManagedState, UnmanagedState, F, A], Precepte[Ta, ManagedState, UnmanagedState, F, A]#S)]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A]

private case class ReturnStep[Ta, ManagedState, UnmanagedState, F[_], A](
  v: (Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, A)
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A]

private case class ApplyStep[Ta, ManagedState, UnmanagedState, F[_], A, B, C](
  pa: Precepte[Ta, ManagedState, UnmanagedState, F, A],
  pf: Precepte[Ta, ManagedState, UnmanagedState, F, A => B],
  next: B => Precepte[Ta, ManagedState, UnmanagedState, F, C]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, C]

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

  @tailrec private final def resume(idx: Int)(state: S)
    (implicit fu: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState])
    : ResumeStep[Ta, ManagedState, UnmanagedState, F, A] = {
    this match {
        case Return(a) =>
          ReturnStep((state, a))

        case Step(st, tags) =>
          val state0 = upd.appendTags(state, tags, idx)
          // append tags to managed state and propagate this new managed state to next step
          SimpleStep(st.run(state0).map { case (s, p) => p -> s })

        case Apply(pa, pf) =>
          ApplyStep(pa, pf, (a: A) => Return(a))

        case f@Flatmap(sub, next) =>
          sub() match {
            case Return(a) =>
              next(a).resume(idx)(state)

            case Step(st, tags) =>
              val state0 = upd.appendTags(state, tags, idx)
              // repass state as a Step in a Flatmap means the flatMap chain is finished
              // Do not reuse appended segment but original state
              FlatMapStep(st.run(state0).map { case (s, p) =>
                (p, next, s)
              })

            case f@Flatmap(sub2, next2) =>
              (Flatmap(sub2, (z: f._I) => next2(z).flatMap(next)):Precepte[Ta, ManagedState, UnmanagedState, F, A]).resume(idx)(state)

            case Apply(pa, pfa) =>
              ApplyStep(pa, pfa, next)
          }
      }
    }

    final def scan[T]
      (append: (S, T) => T, merge: (T, T) => T, appendT: (T, T) => T, subG: (S, T) => T, isEmpty: T => Boolean, empty: T)
      (state: S, t: T, idx: Int = 0)
      (implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState])
      : F[(S, A, T)] = {
      def stepScan[B](p: PX[B], state: S, t: T, idx: Int = 0, isRoot: Boolean = false): F[(S, B, T)] = {
        p.resume(idx)(state) match {

          case SimpleStep(fsp) =>
            fsp.flatMap { case (p0, s0) =>

              stepScan(p0, s0, empty).map { case (s1, a1, t1) =>

                val t2 = if(isEmpty(t1)) {
                  append(s0, t)
                } else {
                  if(!isRoot) {
                    appendT(t, subG(s0, t1))
                  } else {
                    appendT(append(s0, t), t1)
                  }
                }
                (s1, a1, t2)
              }
            }

          case FlatMapStep(fsp) =>
            fsp.flatMap { case (p0, next0, s0) =>
              stepScan(p0, s0, empty).flatMap { case (s1, a1, t1) =>
                val t2 = if(isEmpty(t1)) {
                  append(s0, t)
                } else {
                  if(!isRoot) {
                    appendT(t, subG(s0, t1))
                  } else {
                    appendT(append(s0, t), t1)
                  }
                }
                stepScan(next0(a1), s1, t2)
              }
            }

          case ReturnStep(sat) =>
            sat.point[F].map { case (s, a) => (s, a, t) }

          case ApplyStep(pa, pf, next) =>
            (stepScan(pa, state, empty, idx + 1) |@| stepScan(pf, state, empty, idx + 2)).tupled.map {
              case ((s0, a, t0), (s1, f, t1)) =>
                val s = upd.updateUnmanaged(s0, S.append(s0.unmanaged, s1.unmanaged))
                val tm = merge(t0, t1)
                val tf = appendT(t, tm)
                (s, f(a), tf)
            }.flatMap { case (s0, b, t0) =>
              stepScan(next(b), s0, t0, idx)
            }
        }
      }

      stepScan(this, state, t, idx, true)
    }

    final def run(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[(S, A)] =
      scan[Unit]((_, _) => (), (_, _) => (), (_, _) => (), (_, _) => (), _ => true, ())(state, ()).map{ case (s, a, t) => (s, a) }

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
        val g = Graph(nodes, edges)
        g -> g.children.map(_.id)
      }

      def appendT(g0: G, g1: G): G = {
        val nodes = g0._1.nodes ++ g1._1.nodes
        val edges = g0._1.edges ++ g1._1.edges

        val newEdges = for {
          c <- g0._1.children
          p <- g1._1.parents
        } yield Edge(c.id, p.id)

        Graph(nodes, edges ++ newEdges) -> g1._1.children.map(_.id)
      }

      def sub(s: S, g: G): G = {
        val node = nod.toNode(s)
        Graph(Set(Sub("cluster_" + node.id, node.value, g._1)), Set.empty) -> Set(node.id)
      }

      val empty: G = Graph(Set(), Set()) -> Set()

      def isEmpty(g: G) = g match {
        case `empty` => true
        case _ => false
      }

      scan[G](append _, merge _, appendT _, sub _, isEmpty _, empty)(state, empty).map { case (s, a, g) => (s, a, g._1) }
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

      override def ap[A, B](pa: => Precepte[Ta, ManagedState, UnmanagedState, F, A])(pab: => Precepte[Ta, ManagedState, UnmanagedState, F, A => B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] = {
        Apply(pa, pab)
      }
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