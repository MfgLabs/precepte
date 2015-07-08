package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Bind, Monad, MonadPlus, Applicative, Functor, \/, \/-, -\/, IndexedStateT, StateT, State, Traverse, ~> }
import scalaz.Isomorphism.{<~>, <=>, isoRefl, isoNaturalRefl}
import scalaz.syntax.monad._

import scala.annotation.tailrec
import scalaz.{TreeLoc, Tree}

class TaggingContext[Tags, ManagedState, UnmanagedState, F[_]] {
  self =>

  type S = PState0[Tags, ManagedState, UnmanagedState]

  type StepState[A] = StateT[F, S, Precepte[A]]

  trait TaggingContextIso[F2[_]] {

    val tc: TaggingContext[Tags, ManagedState, UnmanagedState, F2]

    val iso: self.Precepte <~> tc.Precepte
  }

  trait TaggingContextIso0[Tags, ManagedState, UnmanagedState, F2[_]] {

    val tc: TaggingContext[Tags, ManagedState, UnmanagedState, F2]

    val iso: self.Precepte <~> tc.Precepte
  }

  def isoF[F2[_]](isoFF: F <~> F2)(implicit mf: Monad[F], mf2: Monad[F2]) =
    iso0[Tags, ManagedState, UnmanagedState, F2](isoRefl, isoRefl, isoFF)

  def isoState[Tags2, ManagedState2, UnmanagedState2](isoT: Tags <=> Tags2, isoS: S <=> PState0[Tags2, ManagedState2, UnmanagedState2])(implicit mf: Monad[F]) =
    iso0[Tags2, ManagedState2, UnmanagedState2, F](isoT, isoS, isoNaturalRefl)

  def isoState[ManagedState2, UnmanagedState2](isoS: S <=> PState0[Tags, ManagedState2, UnmanagedState2])(implicit mf: Monad[F]) =
    iso0[Tags, ManagedState2, UnmanagedState2, F](isoRefl, isoS, isoNaturalRefl)

  def isoState[ManagedState2, UnmanagedState2](toS2: S => PState0[Tags, ManagedState2, UnmanagedState2], fromS2: PState0[Tags, ManagedState2, UnmanagedState2] => S)(implicit mf: Monad[F]) =
    iso0[Tags, ManagedState2, UnmanagedState2, F](isoRefl, new (S <=> PState0[Tags, ManagedState2, UnmanagedState2]) {
      def to = toS2
      def from = fromS2
    }, isoNaturalRefl)

  def isoUnmanagedState[UnmanagedState2](toS2: UnmanagedState => UnmanagedState2, fromS2: UnmanagedState2 => UnmanagedState)(implicit mf: Monad[F]) =
    iso0[Tags, ManagedState, UnmanagedState2, F](isoRefl, new (S <=> PState0[Tags, ManagedState, UnmanagedState2]) {
      def to = (s: S) => PState0[Tags, ManagedState, UnmanagedState2](managed = s.managed, unmanaged = toS2(s.unmanaged))
      def from = (s2: PState0[Tags, ManagedState, UnmanagedState2]) => PState0(managed = s2.managed, unmanaged = fromS2(s2.unmanaged))
    }, isoNaturalRefl)

  def iso0[Tags2, ManagedState2, UnmanagedState2, F2[_]](
    isoT: Tags <=> Tags2, isoS: S <=> PState0[Tags2, ManagedState2, UnmanagedState2], isoF: F <~> F2
  )(implicit mf: Monad[F], mf2: Monad[F2]) = new TaggingContextIso0[Tags2, ManagedState2, UnmanagedState2, F2] {

    override val tc = new TaggingContext[Tags2, ManagedState2, UnmanagedState2, F2]

    override lazy val iso: self.Precepte <~> tc.Precepte = new (self.Precepte <~> tc.Precepte) {

      def to = new (self.Precepte ~> tc.Precepte) {
        def apply[A](p: self.Precepte[A]): tc.Precepte[A] = p match {

          case self.Return(a) =>
            tc.Return(a)

          case self.Step(st, tags) =>
            tc.Step[A](
              IndexedStateT { (s2: tc.S) =>
                isoF.to(st(isoS.from(s2)).map{ case (s, p) => isoS.to(s) -> iso.to(p) })
              }, isoT.to(tags)
            )

          case f@self.Flatmap(sub, next) =>
            tc.Flatmap(() => iso.to(sub()), (i:f._I) => iso.to(next(i)))

          case f@self.FlatmapK(subk, fk) =>
            tc.FlatmapK(
              iso.to(subk),
              (f2: F2[(tc.S, f._A)]) => iso.to(fk(isoF.from(f2.map{ case (s2, fa) => isoS.from(s2) -> fa })))
            )

        }
      }

      def from = new (tc.Precepte ~> self.Precepte) {
        def apply[A](p2: tc.Precepte[A]): self.Precepte[A] = p2 match {

          case tc.Return(a) =>
            self.Return(a)

          case tc.Step(st, tags) =>
            self.Step[A](
              IndexedStateT { (s: S) =>
                isoF.from(st(isoS.to(s)).map{ case (s, p) => isoS.from(s) -> iso.from(p) })
              }, isoT.from(tags)
            )

          case f@tc.Flatmap(sub, next) =>
            self.Flatmap(() => iso.from(sub()), (i:f._I) => iso.from(next(i)))

          case f@tc.FlatmapK(subk, fk) =>
            self.FlatmapK(
              iso.from(subk),
              (ff: F[(S, f._A)]) => iso.from(fk(isoF.to(ff.map { case (s, fa) => isoS.to(s) -> fa })))
            )

        }
      }
    }
  }

  trait ResumeStep[A]
  case class FlatMapStep[A](v: F[(Precepte[A], S)]) extends ResumeStep[A]
  case class ReturnStep[A](v: (S, A)) extends ResumeStep[A]
  case class KStep[A](v: PrecepteK[A]) extends ResumeStep[A]

  // trait ResumeGraphStep[G <: Graph[T, S, G], A]
  // case class FlatMapGraphStep[G <: Graph[T, S, G], A](v: F[(Precepte[A], S, PIdSeries, G)]) extends ResumeGraphStep[G, A]
  // case class ReturnGraphStep[G <: Graph[T, S, G], A](v: (A, S, PIdSeries, G)) extends ResumeGraphStep[G, A]
  // case class KGraphStep[G <: Graph[T, S, G], A](v: PrecepteK[A]) extends ResumeGraphStep[G, A]

  // trait ResumeTreeStep[A]
  // case class ReturnTreeStep[A](v: (A, S, PIdSeries, TreeLoc[Node])) extends ResumeTreeStep[A]
  // case class STreeStep[A](v: F[(Precepte[A], S, PIdSeries, TreeLoc[Node])]) extends ResumeTreeStep[A]
  // case class FlatMapTreeStep[A](v: F[(Precepte[A], S, PIdSeries, TreeLoc[Node], Int, Int)]) extends ResumeTreeStep[A]
  // case class KTreeStep[A](v: PrecepteK[A]) extends ResumeTreeStep[A]

  sealed trait Precepte[A] {
    self =>

    final def flatMap[B](f: A => Precepte[B]): Precepte[B] =
      Flatmap[A, B](() => self, f)

    final def map[B](f: A => B): Precepte[B] =
      flatMap(a => Return(f(a)))

    final def flatMapK[B](f: F[(S, A)] => Precepte[B]): Precepte[B] =
      FlatmapK(self, f)

    /** alias for flatMapK */
    final def introspect[B](f: F[(S, A)] => Precepte[B]): Precepte[B] = flatMapK(f)

    final def mapK[B](f: F[(S, A)] => F[B])(implicit F: Functor[F]): Precepte[B] =
      MapK(self, f)

    def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Precepte[AP[A]] =
      this.map(a => ap.point(a))

    @tailrec private final def resume(state: S)(implicit fu: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): ResumeStep[A] = this match {
      case Return(a) =>
        ReturnStep((state, a))

      case Step(st, tags) =>
        val state0 = upd.appendTags(state, tags)
        // append tags to managed state and propagate this new managed state to next step
        FlatMapStep(st.run(state0).map { case (s, p) => (p, s) })

      case Flatmap(sub, next) =>
        sub() match {
          case Return(a) =>
            next(a).resume(state)

          case Step(st, tags) =>
            val state0 = upd.appendTags(state, tags)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            // Do not reuse appended segment but original state
            FlatMapStep(st.run(state0).map { case (s, p) =>
              (p.flatMap(next), upd.updateUnmanaged(state, s.unmanaged))
            })

          case f@Flatmap(sub2, next2) =>
            (Flatmap(sub2, (z:f._I) => next2(z).flatMap(next)):Precepte[A]).resume(state)

          case MapK(subk, fk) =>
            subk.mapK(z => fk(z).map(next)).flatMap(identity).resume(state)

          case FlatmapK(subk, fk) =>
            subk.flatMapK(z => fk(z).flatMap(next)).resume(state)
        }

      case k@MapK(_,_) =>
        KStep(k)

      case k@FlatmapK(_,_) =>
        KStep(k)
    }

    final def run(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[(S, A)] = {
      this.resume(state) match {
        case FlatMapStep(fsp) =>
          fsp.flatMap { case (p0, s0) => p0.run(s0) }

        case ReturnStep(asi) => asi.point[F]

        case KStep(p) =>
          p match {
            case FlatmapK(subk, fk) =>
              val f = subk.run(state)
              // retry/recover with last state/ids
              fk(f).run(state)

            case MapK(subk, fk) =>
              val f = subk.run(state)
              // retry/recover with last state/ids
              fk(f).map(a => state -> a)
          }


      }
    }

    final def eval(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[A] =
      this.run(state).map(_._2)

    final def observe(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[(A, S, PIdSeries, TreeLoc[Node])] = ???


    /*@tailrec private final def resumeStepState(state: S)(implicit fu: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState, S]): ResumeStepState[A] = this match {
      case Return(a) =>
        // println(s"EVAL R $this")
        ReturnStepStateResume((a, state))

      case Step(st, tags) =>
        // println(s"EVAL S $this")
        val state0 = upd.appendTags(state, tags)
        // val (state0, ids0) = ps.run(state, ids, tags)
        // append tags to managed state and propagate this new managed state to next step
        FlatMapStep(st.run(state0).map { case (s, p) => (p, s) })

      case Flatmap(sub, next) =>
        // println(s"EVAL Flatmap $this")
        sub() match {
          case Return(a) =>
            // println("EVAL Flatmap - Return")
            next(a).resume(state)

          case Step(st, tags) =>
            // println("EVAL Flatmap - Step")
            val state0 = upd.appendTags(state, tags)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            // DO NOT APPEND TAGS TO
            FlatMapStep(st.run(state0).map { case (s, p) =>
              (p.flatMap(next), upd.updateUnmanaged(state, s.unmanaged))
            })

          case f@Flatmap(sub2, next2) =>
            // println("EVAL Flatmap - Flatmap")
            (Flatmap(sub2, (z:f._I) => next2(z).flatMap(next)):Precepte[A]).resume(state)

          case MapK(subk, fk) =>
            // println("EVAL Flatmap - MapK")
            subk.mapK(z => fk(z).map(next)).flatMap(identity).resume(state)

          case FlatmapK(subk, fk) =>
            // println("EVAL Flatmap - FlatmapK")
            subk.flatMapK(z => fk(z).flatMap(next)).resume(state)
        }

      case k@MapK(_,_) =>
        KStep(k)

      case k@FlatmapK(_,_) =>
        KStep(k)
    }*/

    final def mapStep(f: StepState ~> StepState)(implicit F: Functor[F]): Precepte[A] =
      this match {
        case Return(a) =>
          Return(a)

        case Step(st, tags) =>
          Step(f(st).map(_.mapStep(f)), tags)

        case fl@Flatmap(sub, next) =>
          Flatmap(() => sub().mapStep(f), (n: fl._I) => next(n).mapStep(f))

        case MapK(sub, next) => MapK(sub.mapStep(f), next)

        case k@FlatmapK(sub, next) => FlatmapK(sub.mapStep(f), (fa: F[(S, k._A)]) => next(fa).mapStep(f))
      }


    // @tailrec private final def resumeObserve[G <: Graph[T, S, G]](state: S, ids: PIdSeries, graph: G)(
    //   implicit fu: Monad[F], psg: PGraphStatable[T, S], rt: Rootable[T, S]
    // ): ResumeGraphStep[G, A] = this match {
    //   case Return(a) =>
    //     // println("R")
    //     ReturnGraphStep((a, state, ids, graph))

    //   case Step(st, tags) =>
    //     // println("S")
    //     // add segment and retrieve new idseries
    //     val (s0, ids0, g0) = psg.run(state, ids, tags)
    //     // eval new state
    //     FlatMapGraphStep(st.run(s0).map { case (s, p) => (p, s, ids, graph.addChild(g0)) })


    //   case Flatmap(sub, next) =>
    //     // println("Flatmap")
    //     sub() match {
    //       case Return(a) =>
    //         // println("Flatmap - Return")
    //         next(a.asManagedStatetanceOf[Any]).resumeObserve(state, ids, graph)

    //       case Step(st, tags) =>
    //         // println("Flatmap - Step")
    //         val (s0, ids0, g0) = psg.run(state, ids, tags)
    //         // repass state as a Step in a Flatmap means the flatMap chain is finished
    //         FlatMapGraphStep(st.run(s0).map { case (s, p) => (p.flatMap(next), s, ids0, graph.addChild(g0)) })

    //       case f:Flatmap[i, a] =>
    //         // (sub2, next2)
    //         // println("Flatmap - Flatmap")
    //         (Flatmap(f.sub, (z:i) => f.next(z).flatMap(next)):Precepte[A]).resumeObserve(state, ids, graph)
    //         // sub2().flatMap(z => next2(z).flatMap(next)).resumeObserve(state, ids, graph)

    //       case MapK(subk, fk) =>
    //         // println("Flatmap - MapK")
    //         subk.mapK(z => fk(z).map(next)).flatMap(identity).resumeObserve(state, ids, graph)

    //       case FlatmapK(subk, fk) =>
    //         // println("Flatmap - FlatmapK")
    //         subk.flatMapK(z => fk(z).flatMap(next)).resumeObserve(state, ids, graph)
    //     }

    //   case k@MapK(_, _) =>
    //     KGraphStep(k)

    //   case k@FlatmapK(_, _) =>
    //     KGraphStep(k)

    // }

    // final def observe(state: S, ids: PIdSeries = PIdStream())(implicit mo: Monad[F], psg: PGraphStatable[T, S], rt: Rootable[T, S]): F[(A, S, PIdSeries, Root[T, S])] = {

    //   def go[G <: Graph[T, S, G], B](p: Precepte[B], state: S, ids: PIdSeries, graph: G): F[(B, S, PIdSeries, G)] = {

    //     p.resumeObserve(state, ids, graph) match {
    //       case FlatMapGraphStep(fsp) =>
    //         fsp.flatMap { case (p0, s0, ids0, g0) => go(p0, s0, ids0, g0) }

    //       case ReturnGraphStep(asi) =>
    //         asi.point[F]

    //       case KGraphStep(p) =>
    //         p match {
    //           case FlatmapK(subk, fk) =>
    //             val f = go(subk, state, ids, graph)
    //             // retry/recover with last state/ids
    //             go(fk(f.map(_._1)), state, ids, graph)

    //           case MapK(subk, fk) =>
    //             val f = go(subk, state, ids, graph)
    //             // retry/recover with last state/ids
    //             fk(f.map(_._1)).map(a => (a, state, ids, graph))
    //         }

    //       }
    //   }

    //   go(this, state, ids, rt.toRoot(state))
    // }


    /*private def applyRoot(zipper: TreeLoc[Node], nbSub: Int) =
      (1 to nbSub).foldLeft(zipper){ case (z, i) => println(s"$i"); z.root }

    @tailrec private final def resumeObserve0(state: S, ids: PIdSeries, zipper: TreeLoc[Node], nbSub: Int = 0, cur: Int = 0)(implicit fu: Monad[F], psg: PGraphStatable[T, S], rt: Rootable[T, S]): ResumeTreeStep[A] = this match {
      case Return(a) =>
        println("R")
        ReturnTreeStep((a, state, ids, zipper))

      case Step(st, tags) =>
        println(s"Step $this $nbSub $cur")
        val (s0, ids0, g0) = psg.run(state, ids, tags)
        STreeStep(st.run(s0).map { case (s, p) =>
          (
            p, s, ids0,
            if (cur == 0 && nbSub == 0) zipper.ManagedStateertDownLast(Tree.leaf(Node0(g0.id, tags)))
            else zipper.ManagedStateertRight(Tree.leaf(Node0(g0.id, tags)))
          )
        })


      case f@Flatmap(sub, next) =>
        println(s"Flatmap $f $nbSub $cur")
        sub() match {
          case Return(a) =>
            println("Flatmap - Return")
            if(cur <= nbSub) next(a.asManagedStatetanceOf[Any]).resumeObserve0(state, ids, zipper, nbSub, cur)
            else next(a.asManagedStatetanceOf[Any]).resumeObserve0(state, ids, applyRoot(zipper, nbSub), 0, 0)

          case Step(st, tags) =>
            // println("Flatmap - Step")
            val (s0, ids0, g0) = psg.run(state, ids, tags)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            FlatMapTreeStep(
              st.run(s0).map { case (s, p) =>
                ( p.flatMap(next),
                  s,
                  ids0,
                  // embedded flatmap => right
                  if(cur > 1) { println("right"); zipper.ManagedStateertRight(Tree.leaf(Node0(g0.id, tags))) }
                  // flatmap at 1st level just under root => right
                  else if (cur == nbSub && zipper.parent.isDefined) { println("right0"); zipper.ManagedStateertRight(Tree.leaf(Node0(g0.id, tags))) }
                  // else => down last
                  else { println("down"); zipper.ManagedStateertDownLast(Tree.leaf(Node0(g0.id, tags))) },
                  nbSub,
                  if(cur > 1) cur + 1
                  else if (cur == nbSub && zipper.parent.isDefined) cur
                  else cur + 1
                )
              }
            )

          case f:Flatmap[i, a] =>
            // println("Flatmap - Flatmap")
            (Flatmap(f.sub, (z:i) => f.next(z).flatMap(next)):Precepte[A]).resumeObserve0(state, ids, zipper, nbSub + 1, cur)
            // sub2.flatMap(z => next2(z).flatMap(next)).resumeObserve0(state, ids, zipper, nbSub + 1, cur)

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
  */
  }

  case class Return[A](a: A) extends Precepte[A]

  case class Step[A](st: StepState[A], tags: Tags) extends Precepte[A]

  case class Flatmap[I, A](sub: () => Precepte[I], next: I => Precepte[A]) extends Precepte[A] {
    type _I = I
  }

  trait PrecepteK[A] extends Precepte[A]

  case class MapK[A, B](sub: Precepte[A], f: F[(S, A)] => F[B]) extends PrecepteK[B]

  case class FlatmapK[A, B](sub: Precepte[A], f: F[(S, A)] => Precepte[B]) extends PrecepteK[B] {
    type _A = A
  }

  trait LowPriorityManagedStatetances {
    implicit def precepteMonadManagedStatetance(implicit B: Applicative[F]) =
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
              fa <*> fab.map { case (s, ab) => (s: (S, A)) => ab(s._2) }
            }
          }
      }

  }

  object Precepte extends LowPriorityManagedStatetances {

    trait PrecepteBuilder {
      val tags: Tags
      // import scalaz.Id._

      // def apply0[E <: Env, C, A](λ: State[E, T, C] => A): Precepte[E, T, C, Id, A] =
      //   apply[E, C, Id, A](λ)

      def apply[A](λ: S => F[A])(implicit F: Functor[F]): Precepte[A] =
        Step[A](
          IndexedStateT { (st: S) =>
            for (a <- λ(st))
            yield st -> Return(a)
          }, tags)

      def applyS[A](λ: S => F[(UnmanagedState, A)])(implicit F: Functor[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): Precepte[A] =
        Step[A](
          IndexedStateT { (st: S) =>
            for (ca <- λ(st))
            yield {
              val (unmanaged, a) = ca
              upd.updateUnmanaged(st, unmanaged) -> Return(a)
            }
          }, tags)

      def apply[A](m: Precepte[A])(implicit A: Applicative[F]): Precepte[A] =
        Step(IndexedStateT[F, S, S, Precepte[A]]{ st =>
          (st -> m).point[F]
        }, tags)
    }

    def apply(_tags: Tags) =
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