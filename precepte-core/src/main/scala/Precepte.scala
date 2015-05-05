package com.mfglabs.monitoring

import scala.language.higherKinds
import scalaz.{ Bind, Monad, MonadPlus, Applicative, Functor, \/, \/-, -\/, IndexedStateT }
import scalaz.syntax.monad._
import Call.{ Env, Tags }

import Precepte.Aux

sealed trait Precepte[A] {
  _self =>

  type E <: Env
  type T <: Tags
  type C
  type F[_]

  val self = _self.asInstanceOf[Aux[E, T, C, F, A]]

  final def flatMap[B](f: A => Aux[E, T, C, F, B]): Aux[E, T, C, F, B] = {
    val x: Aux[E, T, C, F, B] = Flatmap[E, T, C, F, A, B](self, f)
    x
  }

  final def map[B](f: A => B): Aux[E, T, C, F, B] =
    flatMap(a => Return[E, T, C, F, B](f(a)))

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

  final def flatMapK[B](f: F[A] => F[Aux[E, T, C, F, B]]): Aux[E, T, C, F, B] =
    FlatmapK[E, T, C, F, A, B](self, f)

  def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Aux[E, T, C, F, AP[A]] =
    this.map(a => ap.point(a))


  final def eval(state: Call.State[E, T, C], ids: Stream[Call.Id] = Stream.continually(Call.Id.gen))(implicit mo: Monad[F]): F[A] = {
    this match {
      case Return(a) => a.point[F]
      case Step(st, tags) =>
        val state0 = state.copy(path = (state.path :+ Call(ids.head, tags)).asInstanceOf[Call.Path[T]]) // XXX
        st.asInstanceOf[IndexedStateT[F, Call.State[E, T, C], C, Aux[E, T, C, F, A]]]
          .run(state0).flatMap { case (c, m) =>
            m.eval(state0.copy(value = c), ids.tail)
          }
      case fl @ FlatmapK(sub, f) =>
        ???
        // f.asInstanceOf[fl._A => Aux[E, T, C, F, A]](sub.asInstanceOf[Aux[E, T, C, F, fl._A]].eval(state, ids))
        //   .flatMap { s =>
        //     s.eval(state, ids.tail) // XXX: not sure
        //   }
      case Flatmap(sub, next) =>
        ???
        // sub.eval(state, ids).flatMap { case i =>
        //   next(i).eval(state, ids.tail)
        // }
    }
  }

  final def run(state: Call.State[E, T, C], ids: Stream[Call.Id] = Stream.continually(Call.Id.gen))(implicit mo: Monad[F]): F[(Call.Root[T, C], A)] = {
    ???
    /*
    def go[G <: Call.Graph[T, C, G] ,B](m: Aux[E, T, C, F, B], state: Call.State[E, T, C], graph: G, ids: Stream[Call.Id]): F[(Stream[Call.Id], (G, B))] = {
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
    */
  }

}

case class Return[E0 <: Env, T0 <: Tags, C0, F0[_], A](a: A) extends Precepte[A] {
  type E = E0
  type T = T0
  type C = C0
  type F[X] = F0[X]
}

case class Step[E0 <: Env, T0 <: Tags, C0, F0[_], A](st: IndexedStateT[F0, Call.State[E0, T0, C0], C0, Aux[E0, T0, C0, F0, A]], tags: T0) extends Precepte[A] {
  type E = E0
  type T = T0
  type C = C0
  type F[X] = F0[X]

  def run(state: Call.State[E, T, C]): F[(C, Aux[E, T, C, F, A])] =
    st.run(state)
}

case class Flatmap[E0 <: Env, T0 <: Tags, C0, F0[_], I, A](sub: Aux[E0, T0, C0, F0, I], next: I => Aux[E0, T0, C0, F0, A]) extends Precepte[A] {
  type E = E0
  type T = T0
  type C = C0
  type F[X] = F0[X]

  type _I = I
}

case class FlatmapK[E0 <: Env, T0 <: Tags, C0, F0[_], A, B](sub: Aux[E0, T0, C0, F0, A], f: F0[A] => F0[Aux[E0, T0, C0, F0, B]]) extends Precepte[B] {
  type E = E0
  type T = T0
  type C = C0
  type F[X] = F0[X]

  type _A = A
  type _B = B
}

trait LowPriorityInstances {
  implicit def precepteInstances[E <: Env, T <: Tags, C, F[_]: Bind] =
    new Monad[({ type λ[α] = Aux[E, T, C, F, α] })#λ] {
      override def point[A](a: => A): Aux[E,T,C,F,A] =
        Return[E, T, C, F, A](a)
      override def map[A, B](m: Aux[E, T, C, F, A])(f: A => B): Aux[E,T,C,F,B] =
        m.map(f)
      override def bind[A, B](m: Aux[E, T, C, F, A])(f: A => Aux[E, T, C, F, B]): Aux[E, T, C, F, B] =
        m.flatMap(f)
    }
}

object Precepte extends LowPriorityInstances {

  type Aux[E0 <: Env, T0 <: Tags, C0, F0[_], A] = Precepte[A] {
    type E = E0
    type T = T0
    type C = C0
    type F[X] = F0[X]
  }

  trait PrecepteBuilder[T <: Tags] {
    val tags: T
    import scalaz.Id._

    def apply0[E <: Env, C, A](λ: Call.State[E, T, C] => A): Aux[E, T, C, Id, A] =
      apply[E, C, Id, A](λ)

    def apply[E <: Env, C, F[_]: Functor, A](λ: Call.State[E, T, C] => F[A]): Aux[E, T, C, F, A] =
      Step[E, T, C, F, A](
        IndexedStateT { (st: Call.State[E, T, C]) =>
          for (a <- λ(st))
          yield st.value -> Return[E, T, C, F, A](a)
        }, tags)

    def applyS[E <: Env, C, F[_]: Functor, A](λ: Call.State[E, T, C] => F[(C, A)]): Aux[E, T, C, F, A] =
      Step[E, T, C, F, A](
        IndexedStateT { (st: Call.State[E, T, C]) =>
          for (ca <- λ(st))
          yield {
            val (c, a) = ca
            c -> Return[E, T, C, F, A](a)
          }
        }, tags)

    def apply[E <: Env, C, F[_]: Applicative, A](m: Aux[E, T, C, F, A]): Aux[E, T, C, F, A] =
      ???
      // Step(IndexedStateT[F, Call.State[E, T, C], C, Aux[E, T, C, F, A]]{ st =>
      //   (st.value -> m).point[F]
      // }, tags)
  }

  def apply[T <: Tags](_tags: T) =
    new PrecepteBuilder[T] {
      val tags = _tags
    }

  trait *->*[F[_]] {}
  trait *->*->*[F[_, _]] {}

  implicit def fKindEv[F0[_]] = new *->*[F0] {}
  implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}

  def trans[E <: Env, T <: Tags, C, F[_], G[_]: *->*, A](m: Aux[E, T, C, F, G[A]])(implicit hh: HasHoist[G]): hh.T[({ type λ[α] = Aux[E, T, C, F, α] })#λ, A] = {
    type λ[α] = Aux[E, T, C, F, α]
    hh.lift[λ, A](m)
  }

  def trans[E <: Env, T <: Tags, C, F[_], G[_, _]: *->*->*, A, B](m: Aux[E, T, C, F, G[A, B]])(implicit hh: HasHoist[({ type λ[α] = G[A, α] })#λ]): hh.T[({ type λ[α] = Aux[E, T, C, F, α] })#λ, B] = {
    type λ[α] = G[A, α]
    trans[E, T, C, F, λ, B](m)(new *->*[λ] {}, hh)
  }
}