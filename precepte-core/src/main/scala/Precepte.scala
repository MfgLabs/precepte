package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Bind, Monad, MonadPlus, Applicative, Functor, \/, \/-, -\/, IndexedStateT, StateT, State, Traverse }
import scalaz.syntax.monad._

import scala.annotation.tailrec
import scalaz.{TreeLoc, Tree}

class TaggingContext[T <: Tags, S <: PState[T], F[_]] {

  trait ResumeStep[A]
  case class FlatMapStep[A](v: F[(Precepte[A], S, PIdSeries)]) extends ResumeStep[A]
  case class ReturnStep[A](v: (A, S, PIdSeries)) extends ResumeStep[A]
  case class KStep[A](v: PrecepteK[A]) extends ResumeStep[A]

  trait ResumeGraphStep[G <: Graph[T, S, G], A]
  case class FlatMapGraphStep[G <: Graph[T, S, G], A](v: F[(Precepte[A], S, PIdSeries, G)]) extends ResumeGraphStep[G, A]
  case class ReturnGraphStep[G <: Graph[T, S, G], A](v: (A, S, PIdSeries, G)) extends ResumeGraphStep[G, A]
  case class KGraphStep[G <: Graph[T, S, G], A](v: PrecepteK[A]) extends ResumeGraphStep[G, A]

  trait ResumeTreeStep[A]
  case class ReturnTreeStep[A](v: (A, S, PIdSeries, TreeLoc[Node])) extends ResumeTreeStep[A]
  case class STreeStep[A](v: F[(Precepte[A], S, PIdSeries, TreeLoc[Node])]) extends ResumeTreeStep[A]
  case class FlatMapTreeStep[A](v: F[(Precepte[A], S, PIdSeries, TreeLoc[Node], Int, Int)]) extends ResumeTreeStep[A]
  case class KTreeStep[A](v: PrecepteK[A]) extends ResumeTreeStep[A]

  sealed trait Precepte[A] {
    self =>

    final def flatMap[B](f: A => Precepte[B]): Precepte[B] =
      Flatmap[A, B](self, f)

    final def map[B](f: A => B): Precepte[B] =
      flatMap(a => Return(f(a)))

    final def flatMapK[B](f: F[A] => Precepte[B]): Precepte[B] =
      FlatmapK(self, f)

    /** alias for flatMapK */
    final def introspect[B](f: F[A] => Precepte[B]): Precepte[B] = flatMapK(f)

    final def mapK[B](f: F[A] => F[B])(implicit F: Functor[F]): Precepte[B] =
      MapK(self, f)

    def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Precepte[AP[A]] =
      this.map(a => ap.point(a))

    @tailrec private final def resume(state: S, ids: PIdSeries)(implicit fu: Monad[F], ps: PStatable[T, S]): ResumeStep[A] = this match {
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

    final def run(state: S, ids: PIdSeries = PIdStream())(implicit mo: Monad[F], ps: PStatable[T, S]): F[(A, S, PIdSeries)] = {
      this.resume(state, ids) match {
        case FlatMapStep(fsp) =>
          fsp.flatMap { case (p0, s0, ids0) => p0.run(s0, ids0) }

        case ReturnStep(asi) => asi.point[F]

        case KStep(p) =>
          p match {
            case FlatmapK(subk, fk) =>
              val f = subk.run(state, ids)
              // retry/recover with last state/ids
              fk(f.map(_._1)).run(state, ids)

            case MapK(subk, fk) =>
              val f = subk.run(state, ids)
              // retry/recover with last state/ids
              fk(f.map(_._1)).map(a => (a, state, ids))     
          }
          
          
      }
    }

    final def eval(state: S, ids: PIdSeries = PIdStream())(implicit mo: Monad[F], ps: PStatable[T, S]): F[A] =
      this.run(state, ids).map(_._1)

    @tailrec private final def resumeObserve[G <: Graph[T, S, G]](state: S, ids: PIdSeries, graph: G)(implicit fu: Monad[F], psg: PGraphStatable[T, S], rt: Rootable[T, S]): ResumeGraphStep[G, A] = this match {
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
            next(a.asInstanceOf[Any]).resumeObserve(state, ids, graph)

          case Step(st, tags) =>
            // println("Flatmap - Step")
            val (s0, ids0, g0) = psg.run(state, ids, tags)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            FlatMapGraphStep(st.run(s0).map { case (s, p) => (p.flatMap(next), s, ids0, graph.addChild(g0)) })

          case Flatmap(sub2, next2) =>
            // println("Flatmap - Flatmap")
            sub2.flatMap(z => next2(z).flatMap(next)).resumeObserve(state, ids, graph)

          case MapK(subk, fk) =>
            // println("Flatmap - MapK")
            subk.mapK(z => fk(z).map(next)).flatMap(identity).resumeObserve(state, ids, graph)

          case FlatmapK(subk, fk) =>
            // println("Flatmap - FlatmapK")
            subk.flatMapK(z => fk(z).flatMap(next)).resumeObserve(state, ids, graph)
        }

      case k@MapK(_, _) =>
        KGraphStep(k)

      case k@FlatmapK(_, _) =>
        KGraphStep(k)

    }

    final def observe(state: S, ids: PIdSeries = PIdStream())(implicit mo: Monad[F], psg: PGraphStatable[T, S], rt: Rootable[T, S]): F[(A, S, PIdSeries, Root[T, S])] = {

      def go[G <: Graph[T, S, G], B](p: Precepte[B], state: S, ids: PIdSeries, graph: G): F[(B, S, PIdSeries, G)] = {

        p.resumeObserve(state, ids, graph) match {
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


    def applyRoot(zipper: TreeLoc[Node], nbSub: Int) =
      (1 to nbSub).foldLeft(zipper){ case (z, i) => println(s"$i"); z.root }

    @tailrec private final def resumeObserve0(state: S, ids: PIdSeries, zipper: TreeLoc[Node], nbSub: Int = 0, cur: Int = 0)(implicit fu: Monad[F], psg: PGraphStatable[T, S], rt: Rootable[T, S]): ResumeTreeStep[A] = this match {
      case Return(a) =>
        println("R")
        ReturnTreeStep((a, state, ids, zipper))

      case Step(st, tags) =>
        println("S")
        val (s0, ids0, g0) = psg.run(state, ids, tags)
        STreeStep(st.run(s0).map { case (s, p) => (p, s, ids0, zipper.insertRight(Tree.leaf(Node0(g0.id, tags)))) })


      case f@Flatmap(sub, next) =>
        println(s"Flatmap $f $nbSub $cur")
        sub match {
          case Return(a) =>
            println("Flatmap - Return")
            if(cur <= nbSub) next(a.asInstanceOf[Any]).resumeObserve0(state, ids, zipper, nbSub, cur)
            else next(a.asInstanceOf[Any]).resumeObserve0(state, ids, applyRoot(zipper, nbSub), 0, 0)

          case Step(st, tags) =>
            // println("Flatmap - Step")
            val (s0, ids0, g0) = psg.run(state, ids, tags)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            FlatMapTreeStep(
              st.run(s0).map { case (s, p) =>
                ( p.flatMap(next),
                  s,
                  ids0, 
                  if(cur > 1) { println("right"); zipper.insertRight(Tree.leaf(Node0(g0.id, tags))) }
                  else { println("down"); zipper.insertDownLast(Tree.leaf(Node0(g0.id, tags))) },
                  nbSub,
                  cur + 1
                )
              }
            )

          case Flatmap(sub2, next2) =>
            // println("Flatmap - Flatmap")
            sub2.flatMap(z => next2(z).flatMap(next)).resumeObserve0(state, ids, zipper, nbSub + 1, cur)

          case MapK(subk, fk) =>
            // println("Flatmap - MapK")
            subk.mapK(z => fk(z).map(next)).flatMap(identity).resumeObserve0(state, ids, zipper, nbSub, cur)

          case FlatmapK(subk, fk) =>
            // println("Flatmap - FlatmapK")
            subk.flatMapK(z => fk(z).flatMap(next)).resumeObserve0(state, ids, zipper, nbSub, cur)
        }

      case k@MapK(_, _) =>
        KTreeStep(k)

      case k@FlatmapK(_, _) =>
        KTreeStep(k)

    }


    final def observe0(state: S, ids: PIdSeries = PIdStream())(implicit mo: Monad[F], psg: PGraphStatable[T, S], rt: Rootable[T, S]): F[(A, S, PIdSeries, TreeLoc[Node])] = {

      def go[B](p: Precepte[B], state: S, ids: PIdSeries, zipper: TreeLoc[Node], nbSub: Int = 0, cur: Int = 0): F[(B, S, PIdSeries, TreeLoc[Node])] = {

        p.resumeObserve0(state, ids, zipper, nbSub, cur) match {
          case STreeStep(fsp) =>
            fsp.flatMap { case (p0, s0, ids0, z0) => go(p0, s0, ids0, z0, nbSub, cur) }

          case ReturnTreeStep(asi) =>
            asi.point[F]

          case FlatMapTreeStep(fsp) =>
            fsp.flatMap { case (p0, s0, ids0, z0, nbSub, cur) => go(p0, s0, ids0, z0, nbSub, cur) }

          case KTreeStep(p) =>
            p match {
              case FlatmapK(subk, fk) =>
                val f = go(subk, state, ids, zipper)
                // retry/recover with last state/ids
                go(fk(f.map(_._1)), state, ids, zipper)

              case MapK(subk, fk) =>
                val f = go(subk, state, ids, zipper)
                // retry/recover with last state/ids
                fk(f.map(_._1)).map(a => (a, state, ids, zipper))              
            }
                        
          }
      }

      val root = rt.toRoot(state)
      go(this, state, ids, TreeLoc.loc(Tree.leaf(NodeR(root.span)), Stream.Empty, Stream.Empty, Stream.Empty))//
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