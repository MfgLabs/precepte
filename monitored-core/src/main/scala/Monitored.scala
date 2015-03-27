package com.mfglabs.monitoring

import scala.language.higherKinds
import scalaz.{ Monad, Applicative, Functor, \/, \/-, -\/, IndexedStateT }
import scalaz.syntax.monad._
import Call.Env

sealed trait Monitored[E <: Env, C, F[_], A] {
  self =>

  final def flatMap[B](f: A => Monitored[E, C, F, B]): Monitored[E, C, F, B] =
    Flatmap[E, C, F, A, B](self, f)

  final def map[B](f: A => B): Monitored[E, C, F, B] =
    flatMap(a => Return(f(a)))

  def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Monitored[E, C, F, AP[A]] =
    this.map(a => ap.point(a))

  final def eval(state: Call.State[E, C], ids: Stream[Call.Id] = Stream.continually(Call.Id.gen))(implicit mo: Monad[F]): F[A] = {
    this match {
      case Return(a) => a.point[F]
      case Step(st, tags) =>
        val state0 = state.copy(path = state.path :+ Call(ids.head, tags))
        st.run(state0).flatMap { case(c, m) =>
          m.eval(state0.copy(value = c), ids.tail)
        }
      case Flatmap(sub, next) =>
        sub.eval(state).flatMap { case i =>
          next(i).eval(state)
        }
    }
  }

  final def run(state: Call.State[E, C], ids: Stream[Call.Id] = Stream.continually(Call.Id.gen))(implicit mo: Monad[F]): F[(Call.Root[C], A)] = {
    def go[G <: Call.Graph[C, G] ,B](m: Monitored[E, C, F, B], state: Call.State[E, C], graph: G, ids: Stream[Call.Id]): F[(Stream[Call.Id], (G, B))] = {
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
        case Flatmap(sub, next) =>
          // XXX: kinda hackish. We're only interested in this node children
          val g0 = Call.GraphNode(Call.Id("dummy"), state.value, Call.Tags.empty, Vector.empty)
          go(sub, state, g0, ids).flatMap { case (is0, (gi, i)) =>
            go(next(i), state, gi, is0).map { case (is1, (g, a)) =>
              (is1, (graph.addChildren(g.children), a))
            }
          }
      }
    }
    go(this, state, Call.Root[C](state.span, Vector.empty), ids).map(_._2)
  }

}

private case class Return[E <: Env, C, F[_], A](a: A) extends Monitored[E, C, F, A]
private case class Step[E <: Env, C, F[_], A](st: IndexedStateT[F, Call.State[E, C], C, Monitored[E, C, F, A]], tags: Call.Tags) extends Monitored[E, C, F, A] {
  def run(state: Call.State[E, C]): F[(C, Monitored[E, C, F, A])] =
    st.run(state)
}

private case class Flatmap[E <: Env, C, F[_], I, A](sub: Monitored[E, C, F, I], next: I => Monitored[E, C, F, A]) extends Monitored[E, C, F, A]

object Monitored {

  trait MonitoredBuilder {
    val tags: Call.Tags
    import scalaz.Id._

    def apply0[E <: Env, C, A](λ: Call.State[E, C] => A): Monitored[E, C, Id, A] =
      apply[E, C, Id, A](λ)

    def apply[E <: Env, C, F[_]: Functor, A](λ: Call.State[E, C] => F[A]): Monitored[E, C, F, A] =
      Step[E, C, F, A](
        IndexedStateT { (st: Call.State[E, C]) =>
          for (a <- λ(st))
          yield st.value -> Return(a)
        }, tags)

    def applyS[E <: Env, C, F[_]: Functor, A](λ: Call.State[E, C] => F[(C, A)]): Monitored[E, C, F, A] =
      Step[E, C, F, A](
        IndexedStateT { (st: Call.State[E, C]) =>
          for (ca <- λ(st))
          yield {
            val (c, a) = ca
            c -> Return(a)
          }
        }, tags)

    def apply[E <: Env, C, F[_]: Applicative, A](m: Monitored[E, C, F, A]): Monitored[E, C, F, A] =
      Step(IndexedStateT[F, Call.State[E, C], C, Monitored[E, C, F, A]]{ st =>
        (st.value -> m).point[F]
      }, tags)
  }

  def apply(_tags: Call.Tags) =
    new MonitoredBuilder {
      val tags = _tags
    }

  trait *->*[F[_]] {}
  trait *->*->*[F[_, _]] {}

  implicit def fKindEv[F0[_]] = new *->*[F0] {}
  implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}

  def trans[E <: Env, C, F[_], G[_]: *->*, A](m: Monitored[E, C, F, G[A]])(implicit hh: HasHoist[G]): hh.T[({ type λ[α] = Monitored[E, C, F, α] })#λ, A] = {
    type T[G0] = Monitored[E, C, F, G0]
    hh.lift[T, A](m)
  }

  def trans[E <: Env, C, F[_], G[_, _]: *->*->*, A, B](m: Monitored[E, C, F, G[A, B]])(implicit hh: HasHoist[({ type λ[α] = G[A, α] })#λ]): hh.T[({ type λ[α] = Monitored[E, C, F, α] })#λ, B] = {
    type λ[α] = G[A, α]
    trans[E, C, F, λ, B](m)(new *->*[λ] {}, hh)
  }

  implicit def monitoredInstances[E <: Env, C, F[_]: Monad] =
    new Monad[({ type λ[α] = Monitored[E, C, F, α] })#λ] {
      def point[A](a: => A): Monitored[E, C, F, A] = Monitored(Call.Tags.empty)[E, C, F, A]((_: Call.State[E, C]) => implicitly[Monad[F]].point(a))
      def bind[A, B](m: Monitored[E, C, F, A])(f: A => Monitored[E, C, F, B]): Monitored[E, C, F, B] =
        m.flatMap(f)
    }
}