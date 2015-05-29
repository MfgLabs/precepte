package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Bind, Monad, MonadPlus, Applicative, Functor, \/, \/-, -\/, IndexedStateT, ~> }
import scalaz.syntax.monad._



// class TaggingContext[E <: Env, T <: Tags, C, F[_]] {
class TaggingContext[S <: StateA, F[_]] {
  self =>

  // def contramapE[S2 <: StateA](f: S2 => S)(implicit M: Monad[F]) = {
  //   val tc = new TaggingContext[S2, F]
  //   val nat: self.Precepte ~> tc.Precepte = new (self.Precepte ~> tc.Precepte) {
  //     def apply[A](p: self.Precepte[A]) = p match {
  //       case self.Return(a) => tc.Return(a)
  //       case self.Step(st, tags) =>
  //         tc.Precepte(tags){ state2 => p.eval(state2.mapE(f)) }
  //       // case self.Flatmap(sub, next) =>
  //       //   tc.Flatmap(sub.contramapE(f), i => next(i).contramapE(f))
  //       case _ => throw new RuntimeException("blabla")
  //     }
  //   }
  //   tc -> nat
  // }

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

    final def eval(state: S, ids: Stream[CId] = Stream.continually(CId.gen))(implicit mo: Monad[F]): F[A] = {
      this match {
        case Return(a) => a.point[F]
        case Step(st, tags) =>
          //val state0 = state.copy(path = state.path :+ Call(ids.head, tags))
          val state0 = state.addCall(Call(ids.head, tags.asInstanceOf[state.T0]))
          st.run(state0).flatMap { case (c, m) =>
            m.eval(state0.copyValue(c.asInstanceOf[state.C0]), ids.tail)
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

    final def run(state: S, ids: Stream[CId] = Stream.continually(CId.gen))(implicit mo: Monad[F]): F[(Root[S#T0, S#C0], A)] = {
      def go[G <: Graph[S#T0, S#C0, G], B](m: Precepte[B], state: S, graph: G, ids: Stream[CId]): F[(Stream[CId], (G, B))] = {
        m match {
          case Return(a) =>
            (ids, (graph, a)).point[F]

          case Step(step, tags) =>
            val state0 = state.addCall(Call(ids.head, tags.asInstanceOf[state.T0]))
            step.run(state0).flatMap {
              case (c, mc) =>
                val id = ids.head
                val g0 = GraphNode(id, c, tags, Vector.empty)
                go(mc, state0.copyValue(c.asInstanceOf[state.C0]), g0, ids.tail).map { case (is, (g, a)) =>
                  (is, (graph.addChild(g),  a))
                }
            }
          case FlatmapK(sub, next) =>
            // XXX: kinda hackish. We're only interested in this node children
            val g0 = Root[S#T0, S#C0](Span("dummy"), Vector.empty)
            go(sub, state, g0, ids).flatMap { case (is0, (gi, a)) =>
              next(a.point[F]).flatMap { prb =>
                go(prb, state, gi, is0).map { case (is1, (g, a)) =>
                  (is1, (graph.addChildren(g.children), a))
                }
              }
            }

          case Flatmap(sub, next) =>
            // XXX: kinda hackish. We're only interested in this node children
            val g0 = Root[S#T0, S#C0](Span("dummy"), Vector.empty)
            go(sub, state, g0, ids).flatMap { case (is0, (gi, i)) =>
              go(next(i), state, gi, is0).map { case (is1, (g, a)) =>
                (is1, (graph.addChildren(g.children), a))
              }
            }
        }
      }
      go(this, state, Root[S#T0, S#C0](state.span, Vector.empty), ids).map(_._2)
    }

  }

  case class Return[A](a: A) extends Precepte[A]
  case class Step[A](st: IndexedStateT[F, S, S#C0, Precepte[A]], tags: S#T0) extends Precepte[A] {
    def run(state: S): F[(S#C0, Precepte[A])] =
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
      val tags: S#T0
      // import scalaz.Id._

      // def apply0[E <: Env, C, A](λ: State[E, T, C] => A): Precepte[E, T, C, Id, A] =
      //   apply[E, C, Id, A](λ)

      def apply[A](λ: S => F[A])(implicit F: Functor[F]): Precepte[A] =
        Step[A](
          IndexedStateT { (st: S) =>
            for (a <- λ(st))
            yield st.value -> Return(a)
          }, tags)

      def applyS[A](λ: S => F[(S#C0, A)])(implicit F: Functor[F]): Precepte[A] =
        Step[A](
          IndexedStateT { (st: S) =>
            for (ca <- λ(st))
            yield {
              val (c, a) = ca
              c -> Return(a)
            }
          }, tags)

      def apply[A](m: Precepte[A])(implicit A: Applicative[F]): Precepte[A] =
        Step(IndexedStateT[F, S, S#C0, Precepte[A]]{ st =>
          (st.value.asInstanceOf[S#C0] -> m).point[F]
        }, tags)
    }

    def apply(_tags: S#T0) =
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