package com.mfglabs.monitoring

import scala.language.higherKinds
import scalaz.{ Monad, Applicative, Functor, \/, \/-, -\/, IndexedStateT }
import scalaz.syntax.monad._

sealed trait Monitored[C, F[_], A] {
  self =>

  final def flatMap[B](f: A => Monitored[C, F, B]): Monitored[C, F, B] =
    Flatmap[C, F, A, B](self, f)

  final def map[B](f: A => B): Monitored[C, F, B] =
    flatMap(a => Return(f(a)))

  def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Monitored[C, F, AP[A]] =
    this.map(a => ap.point(a))

  final def eval(state: Call.State[C], ids: Stream[Call.Id] = Stream.continually(Call.Id.gen))(implicit mo: Monad[F]): F[A] = {
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

  final def run(state: Call.State[C], ids: Stream[Call.Id] = Stream.continually(Call.Id.gen))(implicit mo: Monad[F]): F[(Call.Root[C], A)] = {
    def go[G <: Call.Graph[C, G] ,B](m: Monitored[C, F, B], state: Call.State[C], graph: G, ids: Stream[Call.Id]): F[(Stream[Call.Id], (G, B))] = {
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

private case class Return[C, F[_], A](a: A) extends Monitored[C, F, A]
private case class Step[C, F[_], A](st: IndexedStateT[F, Call.State[C], C, Monitored[C, F, A]], tags: Call.Tags) extends Monitored[C, F, A] {
  def run(state: Call.State[C]): F[(C, Monitored[C, F, A])] =
    st.run(state)
}

private case class Flatmap[C, F[_], I, A](sub: Monitored[C, F, I], next: I => Monitored[C, F, A]) extends Monitored[C, F, A]

object Monitored {

  trait MonitoredBuilder {
    val tags: Call.Tags
    import scalaz.Id._

    def apply0[C, A](λ: Call.State[C] => A): Monitored[C, Id, A] =
      apply[C, Id, A](λ)

    def apply[C, F[_]: Functor, A](λ: Call.State[C] => F[A]): Monitored[C, F, A] =
      Step[C, F, A](
        IndexedStateT { (st: Call.State[C]) =>
          for (a <- λ(st))
          yield st.value -> Return(a)
        }, tags)

    def applyS[C, F[_]: Functor, A](λ: Call.State[C] => F[(C, A)]): Monitored[C, F, A] =
      Step[C, F, A](
        IndexedStateT { (st: Call.State[C]) =>
          for (ca <- λ(st))
          yield {
            val (c, a) = ca
            c -> Return(a)
          }
        }, tags)

    def apply[C, F[_]: Applicative, A](m: Monitored[C, F, A]): Monitored[C, F, A] =
      Step(IndexedStateT[F, Call.State[C], C, Monitored[C, F, A]]{ st =>
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

  def trans[C, F[_], G[_]: *->*, A](m: Monitored[C, F, G[A]])(implicit hh: HasHoist[G]): hh.T[({ type λ[α] = Monitored[C, F, α] })#λ, A] = {
    type T[G0] = Monitored[C, F, G0]
    hh.lift[T, A](m)
  }

  def trans[C, F[_], G[_, _]: *->*->*, A, B](m: Monitored[C, F, G[A, B]])(implicit hh: HasHoist[({ type λ[α] = G[A, α] })#λ]): hh.T[({ type λ[α] = Monitored[C, F, α] })#λ, B] = {
    type λ[α] = G[A, α]
    trans[C, F, λ, B](m)(new *->*[λ] {}, hh)
  }

  implicit def monitoredInstances[C, F[_]: Monad] =
    new Monad[({ type λ[α] = Monitored[C, F, α] })#λ] {
      def point[A](a: => A): Monitored[C, F, A] = Monitored(Call.Tags.empty)[C, F, A]((_: Call.State[C]) => implicitly[Monad[F]].point(a))
      def bind[A, B](m: Monitored[C, F, A])(f: A => Monitored[C, F, B]): Monitored[C, F, B] =
        m.flatMap(f)
    }
}