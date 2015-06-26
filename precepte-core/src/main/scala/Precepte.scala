package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Bind, Monad, MonadPlus, Applicative, Functor, \/, \/-, -\/, IndexedStateT, StateT, State, Traverse }
import scalaz.syntax.monad._

import scala.annotation.tailrec

class TaggingContext[T <: Tags, S <: PState[T], F[_]] {
  
  case class ResumeStep[A](result: F[Precepte[A]] \/ A, f: Seq[F[A] => Precepte[A]] = Seq())

  sealed trait Precepte[A] {
    self =>

    final def flatMap[B](f: A => Precepte[B]): Precepte[B] =
      Flatmap[A, B](self, f)

    final def map[B](f: A => B): Precepte[B] =
      flatMap(a => Return(f(a)))

    final def flatMapK[B](f: F[A] => Precepte[B]): Precepte[B] =
      FlatmapK(self, f)

    final def mapK[B](f: F[A] => F[B])(implicit F: Functor[F]): Precepte[B] =
      MapK(self, f)

    def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Precepte[AP[A]] =
      this.map(a => ap.point(a))

    final def resume(state: S, ids: Stream[CId] = Stream.continually(CId.gen), fs: Seq[F[A] => Precepte[A]] = Seq())(implicit fu: Monad[F], ps: PStatable[T, S]): ResumeStep[A] = this match {
      case Return(a) =>
        println("R")
        ResumeStep(\/-(a), fs)

      case Step(st, tags) =>
        println("S")
        val state0 = ps.run(state, ids.head, tags)
        ResumeStep(-\/(st.eval(state0)), fs)

      case Flatmap(sub, next) =>
        println("Flatmap")
        sub match {
          case Return(a) =>
            println("Flatmap - Return")
            next(a.asInstanceOf[Any]).resume(state, ids)
          
          case Step(st, tags) =>
            println("Flatmap - Step")
            val state0 = ps.run(state, ids.head, tags)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            ResumeStep(-\/(st.eval(state0).map(_.flatMap(next))), fs)

          case Flatmap(sub2, next2) =>
            println("Flatmap - Flatmap")
            sub2.flatMap(z => next2(z).flatMap(next)).resume(state, ids)

          case MapK(subk, fk) =>
            println("Flatmap - MapK")
            subk.mapK(z => fk(z).map(next)).flatMap(identity).resume(state, ids)

          case FlatmapK(subk, fk) =>
            println("Flatmap - FlatmapK")
            subk.flatMapK(z => fk(z).flatMap(next)).resume(state, ids)
        }

      case MapK(subk, fk) =>
        println("MapK")
        subk match {
          case Return(a) =>
            println("ZZ0")
            ResumeStep(-\/(fk(a.point[F]).map(Return(_))), fs)
          
          case Step(st, tags) =>
            println("ZZ1")
            val state0 = ps.run(state, ids.head, tags)
            // repass state as a Step in a MapK means the mapK chain is finished
            ResumeStep(-\/(st.eval(state0).map(_.mapK(fk))), fs)

          case Flatmap(sub2, next2) =>  
            println("ZZ2")
            sub2.flatMap(z => next2(z).mapK(fk)).resume(state, ids)

          case MapK(subk2, fk2) =>
            println("ZZ3")
            subk2.mapK(z => fk(fk2(z))).resume(state, ids)

          case FlatmapK(subk2, fk2) =>
            println("ZZ4")
            subk2.flatMapK(z => fk2(z).mapK(fk)).resume(state, ids)
        }

      case FlatmapK(subk, fk) =>
        println("FlatMapK")
        subk.asInstanceOf[Precepte[A]].resume(state, ids, (fs :+ fk).asInstanceOf[Seq[F[A] => Precepte[A]]])
        /*subk match {
          case Return(a) =>
            println("FlatMapK - Return")
            fk(a.point[F]).resume(state, ids)
          
          case Step(st, tags) =>
            println("FlatMapK - Step")
            val state0 = ps.run(state, ids.head, tags)
            // repass state as a Step in a FlatmapK means the flatMapK chain is finished
            -\/(st.eval(state0).map(_.flatMapK(fk)))

          case s@Flatmap(sub, next) =>
            println("FlatMapK - Flatmap")
            s.resume(state, ids) match {
              case -\/(fsp) => -\/(fsp.map(_.flatMapK(fk)))
              case \/-(a) => \/-(a.asInstanceOf[A])
            }

          case MapK(subk2, fk2) =>
            println("FlatMapK - MapK")
            subk2.flatMapK(z => fk(fk2(z))).resume(state, ids)

          case FlatmapK(subk2, fk2) =>
            println("FlatMapK - FlatmapK")
            subk2.flatMapK { z => fk2(z).flatMapK(fk) }.resume(state, ids)
        }*/
    }

    final def eval(state: S, ids: Stream[CId] = Stream.continually(CId.gen), fs: Seq[F[A] => Precepte[A]] = Seq())(implicit mo: Monad[F], ps: PStatable[T, S]): F[A] = {
      this.resume(state, ids) match {
        case ResumeStep(-\/(fsp), Seq()) => fsp.flatMap{ _.eval(state, ids.tail, fs) }
        case ResumeStep(-\/(fsp), fk +: fs) => fk(fsp.flatMap{ _.eval(state, ids.tail, fs) }).eval(state, ids.tail, fs)
        case ResumeStep(\/-(a), Seq()) => a.point[F]
        case ResumeStep(\/-(a), fk +: fs) => fk(a.point[F]).eval(state, ids.tail, fs)
      }

      // @scala.annotation.tailrec
      // def go[B](m: Precepte[B], state: S, ids: Stream[CId]): F[B] = 

      // {
      //   m match {
      //     case Return(a) => a.point[F]

      //     // case ReturnK(fa) => fa

      //     case Step(st, tags) =>
      //       val state0 = ps.run(state, ids.head, tags)
      //       st.run(state0).flatMap { case (s, m) =>
      //         go(m, s, ids.tail)
      //       }

      //     case Flatmap(sub, next) =>
      //       // sub.eval(state, ids)
      //       go(sub, state, ids).flatMap { case i =>
      //         // next(i).eval(state, ids.tail)
      //         go(next(i), state, ids.tail)
      //       }

      //     // case FlatmapK(subk, nextk) =>
      //       // go(nextk(go(subk, state, ids)), 
      //       // .flatMap { s =>
      //       //   // s.eval(state, ids.tail) // XXX: not sure
      //       //   go(s, state, ids.tail)
      //       // }
      //   }
      // }

      // go(this, state, ids)
      /*this match {
        case Return(a) => a.point[F]
        case Step(st, tags) =>
          // val state0 = state.copy(path = state.path :+ Call(ids.head, tags))
          val state0 = ps.run(state, ids.head, tags)
          st.run(state0).flatMap { case (s, m) =>
            m.eval(s, ids.tail)
          }
        case FlatmapK(sub, f) =>
          f(sub.eval(state, ids)).flatMap { s =>
            s.eval(state, ids.tail) // XXX: not sure
          }
        case Flatmap(sub, next) =>
          sub.eval(state, ids).flatMap { case i =>
            next(i).eval(state, ids.tail)
          }
      }*/
    }

    final def run(state: S, ids: Stream[CId] = Stream.continually(CId.gen))(implicit mo: Monad[F], ps: PStatable[T, S], rt: Rootable[T, S]): F[(Root[T, S], A)] = {
      def go[G <: Graph[T, S, G], B](m: Precepte[B], state: S, graph: G, ids: Stream[CId]): F[(Stream[CId], (G, B))] = {
        m match {
          case Return(a) =>
            (ids, (graph, a)).point[F]

          case Step(step, tags) =>
            // val state0 = state.copy(path = state.path :+ Call(ids.head, tags))
            val state0 = ps.run(state, ids.head, tags)
            step.run(state0).flatMap {
              case (s, mc) =>
                val id = ids.head
                val g0 = GraphNode(id, s, tags, Vector.empty)
                // go(mc, state0.copy(value = c), g0, ids.tail).map { case (is, (g, a)) =>
                go(mc, s, g0, ids.tail).map { case (is, (g, a)) =>
                  (is, (graph.addChild(g),  a))
                }
            }
          // case FlatmapK(sub, next) =>
          //   // XXX: kinda hackish. We're only interested in this node children
          //   val g0 = Root[T, S](Span("dummy"), Vector.empty)
          //   go(sub, state, g0, ids).flatMap { case (is0, (gi, a)) =>
          //     next(a.point[F]).flatMap { prb =>
          //       go(prb, state, gi, is0).map { case (is1, (g, a)) =>
          //         (is1, (graph.addChildren(g.children), a))
          //       }
          //     }
          //   }

          case Flatmap(sub, next) =>
            // XXX: kinda hackish. We're only interested in this node children
            val g0 = Root[T, S](Span("dummy"), Vector.empty)
            go(sub, state, g0, ids).flatMap { case (is0, (gi, i)) =>
              go(next(i), state, gi, is0).map { case (is1, (g, a)) =>
                (is1, (graph.addChildren(g.children), a))
              }
            }
        }
      }
      // go(this, state, Root[T, S](state.span, Vector.empty), ids).map(_._2)
      go(this, state, rt.toRoot(state), ids).map(_._2)
    }

    // final def traverse[G[_] : Applicative, B]
    //   (state: S, ids: Stream[CId] = Stream.continually(CId.gen))
    //   (f: A => G[B])
    //   (implicit ft: Traverse[F], mo: Monad[F], ps: PStatable[T, S]): G[Precepte[B]] = {
      
    //   self.resume(state, ids) match {
    //     case -\/(fs) =>
    //       val v: Int = ft.traverseImpl(fs){ case ((s, p)) => p.traverse[G, B](s, ids)(f) }
    //       G.map(v)(Suspend(_))
    //     case \/-(r) => G.map(f(r))(Return(_))
    //   }
    // }

  }

  case class Return[A](a: A) extends Precepte[A]

  // case class ReturnK[A](a: F[A]) extends Precepte[A]

  case class Step[A](st: StateT[F, S, Precepte[A]], tags: T) extends Precepte[A] {
    // def run(state: (S, CId)): F[(C, Precepte[A])] =
    //   st.run(state)
  }

  case class Flatmap[I, A](sub: Precepte[I], next: I => Precepte[A]) extends Precepte[A] {
    type _I = I
  }

  case class MapK[A, B](sub: Precepte[A], f: F[A] => F[B]) extends Precepte[B]

  // case class FlatmapK[A, B](sub: Precepte[A], f: F[A] => F[Precepte[B]]) extends Precepte[B]
  case class FlatmapK[A, B](sub: Precepte[A], f: F[A] => Precepte[B]) extends Precepte[B]

  trait LowPriorityInstances {
    implicit def precepteMonadInstance(implicit B: Applicative[F]) =
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
            }
          }
      }

  }

  object Precepte extends LowPriorityInstances {

    trait PrecepteBuilder {
      val tags: T
      // import scalaz.Id._

      // def apply0[E <: Env, C, A](λ: State[E, T, C] => A): Precepte[E, T, C, Id, A] =
      //   apply[E, C, Id, A](λ)

      def apply[A](λ: S => F[A])(implicit F: Functor[F]): Precepte[A] =
        Step[A](
          IndexedStateT { (st: S) =>
            for (a <- λ(st))
            yield st -> Return(a)
          }, tags)

      def applyS[A](λ: S => F[(S, A)])(implicit F: Functor[F]): Precepte[A] =
        Step[A](
          IndexedStateT { (st: S) =>
            for (ca <- λ(st))
            yield {
              val (c, a) = ca
              c -> Return(a)
            }
          }, tags)

      def apply[A](m: Precepte[A])(implicit A: Applicative[F]): Precepte[A] =
        Step(StateT[F, S, Precepte[A]]{ st =>
          (st -> m).point[F]
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