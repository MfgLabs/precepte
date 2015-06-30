package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Bind, Monad, MonadPlus, Applicative, Functor, \/, \/-, -\/, IndexedStateT, StateT, State, Traverse }
import scalaz.syntax.monad._

import scala.annotation.tailrec

class TaggingContext[T <: Tags, S <: PState[T], F[_]] {

  trait ResumeStep[A]
  case class FlatMapStep[A](v: F[(Precepte[A], S, PIdSeries)]) extends ResumeStep[A]
  case class ReturnStep[A](v: (A, S, PIdSeries)) extends ResumeStep[A]
  case class KStep[A](v: PrecepteK[A]) extends ResumeStep[A]

  trait ResumeGraphStep[G <: Graph[T, S, G], A]
  case class FlatMapGraphStep[G <: Graph[T, S, G], A](v: F[(Precepte[A], S, PIdSeries, G)]) extends ResumeGraphStep[G, A]
  case class ReturnGraphStep[G <: Graph[T, S, G], A](v: (A, S, PIdSeries, G)) extends ResumeGraphStep[G, A]
  case class KGraphStep[G <: Graph[T, S, G], A](v: PrecepteK[A]) extends ResumeGraphStep[G, A]

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

    @tailrec final def resume(state: S, ids: PIdSeries)(implicit fu: Monad[F], ps: PStatable[T, S]): ResumeStep[A] = this match {
      case Return(a) =>
        // println("R")
        ReturnStep((a, state, ids))

      case Step(st, tags) =>
        // println("S")
        val (state0, ids0) = ps.run(state, ids, tags)
        FlatMapStep(st.run(state0).map { case (s, p) => (p, s, ids0) })

      case Flatmap(sub, next) =>
        // println("Flatmap")
        sub match {
          case Return(a) =>
            // println("Flatmap - Return")
            next(a.asInstanceOf[Any]).resume(state, ids)

          case Step(st, tags) =>
            // println("Flatmap - Step")
            val (state0, ids0) = ps.run(state, ids, tags)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            FlatMapStep(st.run(state0).map { case (s, p) => (p.flatMap(next), s, ids0) })

          case Flatmap(sub2, next2) =>
            // println("Flatmap - Flatmap")
            sub2.flatMap(z => next2(z).flatMap(next)).resume(state, ids)

          case MapK(subk, fk) =>
            // println("Flatmap - MapK")
            subk.mapK(z => fk(z).map(next)).flatMap(identity).resume(state, ids)

          case FlatmapK(subk, fk) =>
            // println("Flatmap - FlatmapK")
            subk.flatMapK(z => fk(z).flatMap(next)).resume(state, ids)
        }

      case k@MapK(_,_) =>
        KStep(k)

      case k@FlatmapK(_,_) =>
        KStep(k)
    }

    final def eval0(state: S, ids: PIdSeries = PIdStream())(implicit mo: Monad[F], ps: PStatable[T, S]): F[(A, S, PIdSeries)] = {
      this.resume(state, ids) match {
        case FlatMapStep(fsp) =>
          fsp.flatMap { case (p0, s0, ids0) => p0.eval0(s0, ids0) }

        case ReturnStep(asi) => asi.point[F]

        case KStep(p) =>
          p match {
            case FlatmapK(subk, fk) =>
              val f = subk.eval0(state, ids)
              // retry/recover with last state/ids
              fk(f.map(_._1)).eval0(state, ids)

            case MapK(subk, fk) =>
              val f = subk.eval0(state, ids)
              // retry/recover with last state/ids
              fk(f.map(_._1)).map(a => (a, state, ids))     
          }
          
          
      }
    }

    final def eval(state: S, ids: PIdSeries = PIdStream())(implicit mo: Monad[F], ps: PStatable[T, S]): F[A] =
      this.eval0(state, ids).map(_._1)

    final def resumeGraph[G <: Graph[T, S, G]](state: S, ids: PIdSeries, graph: G)(implicit fu: Monad[F], psg: PGraphStatable[T, S], rt: Rootable[T, S]): ResumeGraphStep[G, A] = this match {
      case Return(a) =>
        // println("R")
        ReturnGraphStep((a, state, ids, graph))

      case Step(st, tags) =>
        // println("S")
        val (s0, ids0, g0) = psg.run(state, ids, tags)
        FlatMapGraphStep(st.run(s0).map { case (s, p) => (p, s, ids, graph.addChild(g0)) })


      case Flatmap(sub, next) =>
        // println("Flatmap")
        sub match {
          case Return(a) =>
            // println("Flatmap - Return")
            next(a.asInstanceOf[Any]).resumeGraph(state, ids, graph)

          case Step(st, tags) =>
            // println("Flatmap - Step")
            val (s0, ids0, g0) = psg.run(state, ids, tags)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            FlatMapGraphStep(st.run(s0).map { case (s, p) => (p.flatMap(next), s, ids0, graph.addChild(g0)) })

          case Flatmap(sub2, next2) =>
            // println("Flatmap - Flatmap")
            sub2.flatMap(z => next2(z).flatMap(next)).resumeGraph(state, ids, graph)

          case MapK(subk, fk) =>
            // println("Flatmap - MapK")
            subk.mapK(z => fk(z).map(next)).flatMap(identity).resumeGraph(state, ids, graph)

          case FlatmapK(subk, fk) =>
            // println("Flatmap - FlatmapK")
            subk.flatMapK(z => fk(z).flatMap(next)).resumeGraph(state, ids, graph)
        }

      case k@MapK(_, _) =>
        KGraphStep(k)

      case k@FlatmapK(_, _) =>
        KGraphStep(k)

    }

    final def runGraph(state: S, ids: PIdSeries = PIdStream())(implicit mo: Monad[F], psg: PGraphStatable[T, S], rt: Rootable[T, S]): F[(A, S, PIdSeries, Root[T, S])] = {

      def go[G <: Graph[T, S, G], B](p: Precepte[B], state: S, ids: PIdSeries, graph: G): F[(B, S, PIdSeries, G)] = {

        p.resumeGraph(state, ids, graph) match {
          case FlatMapGraphStep(fsp) =>
            fsp.flatMap { case (p0, s0, ids0, g0) => go(p0, s0, ids0, g0) }

          case ReturnGraphStep(asi) =>
            asi.point[F]

          case KGraphStep(p) =>
            p match {
              case FlatmapK(subk, fk) =>
                val f = go(subk, state, ids, graph)
                // retry/recover with last state/ids
                go(fk(f.map(_._1)), state, ids, graph)

              case MapK(subk, fk) =>
                val f = go(subk, state, ids, graph)
                // retry/recover with last state/ids
                fk(f.map(_._1)).map(a => (a, state, ids, graph))              
            }
                        
          }
      }

      go(this, state, ids, rt.toRoot(state))
    }

  }

  case class Return[A](a: A) extends Precepte[A]

  // case class ReturnK[A](a: F[A]) extends Precepte[A]

  case class Step[A](st: StateT[F, S, Precepte[A]], tags: T) extends Precepte[A] {
    // def run(state: (S, PId)): F[(C, Precepte[A])] =
    //   st.run(state)
  }

  case class Flatmap[I, A](sub: Precepte[I], next: I => Precepte[A]) extends Precepte[A] {
    type _I = I
  }

  trait PrecepteK[A] extends Precepte[A]

  case class MapK[A, B](sub: Precepte[A], f: F[A] => F[B]) extends PrecepteK[B]

  // case class FlatmapK[A, B](sub: Precepte[A], f: F[A] => F[Precepte[B]]) extends Precepte[B]
  case class FlatmapK[A, B](sub: Precepte[A], f: F[A] => Precepte[B]) extends PrecepteK[B]

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