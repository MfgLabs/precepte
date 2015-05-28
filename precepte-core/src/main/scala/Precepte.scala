package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Bind, Monad, MonadPlus, Applicative, Functor, \/, \/-, -\/, IndexedStateT }
import scalaz.syntax.monad._

class TaggingContext[E <: Env, T <: Tags, C, F[_]] {
  self =>

  def mapEnv[E2 <: Env](env: E => E2): (TaggingContext[E2, T, C, F], self.Precepte[A] => TaggingContext[E2, T, C, F]#Precepte[A]) = {
    val tc = new TaggingContext[E2, T, C, F]
    def f[A]: self.Precepte[A] => tc.Precepte[A] = { (p: self.Precepte[A]) => p match {
      case self.Return(a) => tc.Return(a)
      case self.Step(st, tags) => tc.Step(st.mapE(f), tags)
      case _ => throw new RuntimeException("blabla")
    } }
    tc -> f
  }

  sealed trait Precepte[A] {
    self =>

    final def flatMap[B](f: A => Precepte[B]): Precepte[B] =
      Flatmap[A, B](self, f)

    final def map[B](f: A => B): Precepte[B] =
      flatMap(a => Return(f(a)))

    // final def transform[G[_]](f: F ~> G)(implicit F: Functor[F], G: Functor[G]): Precepte[E, T, C, G, A] =
    //   this match {
    //     case Return(a) => Return(a)
    //     case Step(st, tags) =>
    //       Step(st.mapK(f).map(_ transform f), tags)
    //     case MapK(sub, mapK) =>
    //       ???
    //     case fl@Flatmap(sub, next) =>
    //       Flatmap(sub transform f, (i: fl._I) => next(i).transform(f))
    //   }

    final def flatMapK[B](f: F[A] => F[Precepte[B]]): Precepte[B] =
      FlatmapK(self, f)

    final def mapK[B](f: F[A] => F[B])(implicit F: Functor[F]): Precepte[B] =
      flatMapK(x => f(x).map(Return.apply))

    def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Precepte[AP[A]] =
      this.map(a => ap.point(a))

    final def eval(state: State[E, T, C], ids: Stream[CId] = Stream.continually(CId.gen))(implicit mo: Monad[F]): F[A] = {
      this match {
        case Return(a) => a.point[F]
        case Step(st, tags) =>
          val state0 = state.copy(path = state.path :+ Call(ids.head, tags))
          st.run(state0).flatMap { case (c, m) =>
            m.eval(state0.copy(value = c), ids.tail)
          }
        case FlatmapK(sub, f) =>
          f(sub.eval(state, ids)).flatMap { s =>
            s.eval(state, ids.tail) // XXX: not sure
          }
        case Flatmap(sub, next) =>
          sub.eval(state, ids).flatMap { case i =>
            next(i).eval(state, ids.tail)
          }
      }
    }

    final def run(state: State[E, T, C], ids: Stream[CId] = Stream.continually(CId.gen))(implicit mo: Monad[F]): F[(Root[T, C], A)] = {
      def go[G <: Graph[T, C, G], B](m: Precepte[B], state: State[E, T, C], graph: G, ids: Stream[CId]): F[(Stream[CId], (G, B))] = {
        m match {
          case Return(a) =>
            (ids, (graph, a)).point[F]

          case Step(step, tags) =>
            val state0 = state.copy(path = state.path :+ Call(ids.head, tags))
            step.run(state0).flatMap {
              case (c, mc) =>
                val id = ids.head
                val g0 = GraphNode(id, c, tags, Vector.empty)
                go(mc, state0.copy(value = c), g0, ids.tail).map { case (is, (g, a)) =>
                  (is, (graph.addChild(g),  a))
                }
            }
          case FlatmapK(sub, next) =>
            // XXX: kinda hackish. We're only interested in this node children
            val g0 = Root[T, C](Span("dummy"), Vector.empty)
            go(sub, state, g0, ids).flatMap { case (is0, (gi, a)) =>
              next(a.point[F]).flatMap { prb =>
                go(prb, state, gi, is0).map { case (is1, (g, a)) =>
                  (is1, (graph.addChildren(g.children), a))
                }
              }
            }

          case Flatmap(sub, next) =>
            // XXX: kinda hackish. We're only interested in this node children
            val g0 = Root[T, C](Span("dummy"), Vector.empty)
            go(sub, state, g0, ids).flatMap { case (is0, (gi, i)) =>
              go(next(i), state, gi, is0).map { case (is1, (g, a)) =>
                (is1, (graph.addChildren(g.children), a))
              }
            }
        }
      }
      go(this, state, Root[T, C](state.span, Vector.empty), ids).map(_._2)
    }

  }

  case class Return[A](a: A) extends Precepte[A]
  case class Step[A](st: IndexedStateT[F, State[E, T, C], C, Precepte[A]], tags: T) extends Precepte[A] {
    def run(state: State[E, T, C]): F[(C, Precepte[A])] =
      st.run(state)
  }

  case class Flatmap[I, A](sub: Precepte[I], next: I => Precepte[A]) extends Precepte[A] {
    type _I = I
  }

  case class FlatmapK[A, B](sub: Precepte[A], f: F[A] => F[Precepte[B]]) extends Precepte[B]

  trait LowPriorityInstances {
    implicit def precepteInstances(implicit B: Applicative[F]) =
      new Monad[Precepte] {
        override def point[A](a: => A): Precepte[A] =
          Return(a)
        override def map[A, B](m: Precepte[A])(f: A => B): Precepte[B] =
          m.map(f)
        override def bind[A, B](m: Precepte[A])(f: A => Precepte[B]): Precepte[B] =
          m.flatMap(f)

        // override to support parallel execution
        override def ap[A, B](pa: => Precepte[A])(pab: => Precepte[A => B]) =
          pa.flatMapK { fa =>
            pab.mapK { fab =>
              fa <*> fab
            }.point[F]
          }
      }
  }

  object Precepte extends LowPriorityInstances {

    trait PrecepteBuilder {
      val tags: T
      // import scalaz.Id._

      // def apply0[E <: Env, C, A](λ: State[E, T, C] => A): Precepte[E, T, C, Id, A] =
      //   apply[E, C, Id, A](λ)

      def apply[A](λ: State[E, T, C] => F[A])(implicit F: Functor[F]): Precepte[A] =
        Step[A](
          IndexedStateT { (st: State[E, T, C]) =>
            for (a <- λ(st))
            yield st.value -> Return(a)
          }, tags)

      def applyS[A](λ: State[E, T, C] => F[(C, A)])(implicit F: Functor[F]): Precepte[A] =
        Step[A](
          IndexedStateT { (st: State[E, T, C]) =>
            for (ca <- λ(st))
            yield {
              val (c, a) = ca
              c -> Return(a)
            }
          }, tags)

      def apply[A](m: Precepte[A])(implicit A: Applicative[F]): Precepte[A] =
        Step(IndexedStateT[F, State[E, T, C], C, Precepte[A]]{ st =>
          (st.value -> m).point[F]
        }, tags)
    }

    def apply(_tags: T) =
      new PrecepteBuilder {
        val tags = _tags
      }

    trait *->*[F0[_]] {}
    trait *->*->*[F0[_, _]] {}

    implicit def fKindEv[F0[_]] = new *->*[F0] {}
    implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}

    def trans[G[_]: *->*, A](m: Precepte[G[A]])(implicit hh: HasHoist[G]): hh.T[Precepte, A] =
      hh.lift[Precepte, A](m)

    def trans[G[_, _]: *->*->*, A, B](m: Precepte[G[A, B]])(implicit hh: HasHoist[({ type λ[α] = G[A, α] })#λ]): hh.T[Precepte, B] = {
      type λ[α] = G[A, α]
      trans[λ, B](m)(new *->*[λ] {}, hh)
    }
  }
}