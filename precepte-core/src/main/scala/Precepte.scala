package com.mfglabs.monitoring

import scala.language.higherKinds
import scalaz.{ Bind, Monad, MonadPlus, Applicative, Functor, \/, \/-, -\/, IndexedStateT }
import scalaz.syntax.monad._
import Call.{ Env, Tags }

class TaggingContext[E <: Env, T <: Tags, C, F[_]] {
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

    def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Precepte[AP[A]] =
      this.map(a => ap.point(a))

    final def eval(state: Call.State[E, T, C], ids: Stream[Call.Id] = Stream.continually(Call.Id.gen))(implicit mo: Monad[F]): F[A] = {
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

    final def run(state: Call.State[E, T, C], ids: Stream[Call.Id] = Stream.continually(Call.Id.gen))(implicit mo: Monad[F]): F[(Call.Root[T, C], A)] = {
      def go[G <: Call.Graph[T, C, G] ,B](m: Precepte[B], state: Call.State[E, T, C], graph: G, ids: Stream[Call.Id]): F[(Stream[Call.Id], (G, B))] = {
        m match {
          case Return(a) =>
            (ids, (graph, a)).point[F]
          case Step(step, tags) =>
            val state0 = state.copy(path = state.path :+ Call(ids.head, tags))
            step.run(state0).flatMap {
              case (c, mc) =>
                val id = ids.head
                val g0 = Call.GraphNode(id, c, tags, Vector.empty)
                go(mc, state0.copy(value = c), g0, ids.tail).map { case (is, (g, a)) =>
                  (is, (graph.addChild(g),  a))
                }
            }
          case FlatmapK(sub, f) =>
            ???
          case Flatmap(sub, next) =>
            // XXX: kinda hackish. We're only interested in this node children
            val g0 = Call.Root[T, C](Call.Span("dummy"), Vector.empty)
            go(sub, state, g0, ids).flatMap { case (is0, (gi, i)) =>
              go(next(i), state, gi, is0).map { case (is1, (g, a)) =>
                (is1, (graph.addChildren(g.children), a))
              }
            }
        }
      }
      go(this, state, Call.Root[T, C](state.span, Vector.empty), ids).map(_._2)
    }

  }

  case class Return[A](a: A) extends Precepte[A]
  case class Step[A](st: IndexedStateT[F, Call.State[E, T, C], C, Precepte[A]], tags: T) extends Precepte[A] {
    def run(state: Call.State[E, T, C]): F[(C, Precepte[A])] =
      st.run(state)
  }

  case class Flatmap[I, A](sub: Precepte[I], next: I => Precepte[A]) extends Precepte[A] {
    type _I = I
  }

  case class FlatmapK[A, B](sub: Precepte[A], f: F[A] => F[Precepte[B]]) extends Precepte[B]

  trait LowPriorityInstances {
    implicit def precepteInstances(implicit B: Bind[F]) =
      new Monad[Precepte] {
        override def point[A](a: => A): Precepte[A] =
          Return(a)
        override def map[A, B](m: Precepte[A])(f: A => B): Precepte[B] =
          m.map(f)
        override def bind[A, B](m: Precepte[A])(f: A => Precepte[B]): Precepte[B] =
          m.flatMap(f)
      }
  }

  object Precepte extends LowPriorityInstances {

    trait PrecepteBuilder {
      val tags: T
      // import scalaz.Id._

      // def apply0[E <: Env, C, A](λ: Call.State[E, T, C] => A): Precepte[E, T, C, Id, A] =
      //   apply[E, C, Id, A](λ)

      def apply[A](λ: Call.State[E, T, C] => F[A])(implicit F: Functor[F]): Precepte[A] =
        Step[A](
          IndexedStateT { (st: Call.State[E, T, C]) =>
            for (a <- λ(st))
            yield st.value -> Return(a)
          }, tags)

      def applyS[A](λ: Call.State[E, T, C] => F[(C, A)])(implicit F: Functor[F]): Precepte[A] =
        Step[A](
          IndexedStateT { (st: Call.State[E, T, C]) =>
            for (ca <- λ(st))
            yield {
              val (c, a) = ca
              c -> Return(a)
            }
          }, tags)

      def apply[A](m: Precepte[A])(implicit A: Applicative[F]): Precepte[A] =
        Step(IndexedStateT[F, Call.State[E, T, C], C, Precepte[A]]{ st =>
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

