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

  type S = PState[Tags, ManagedState, UnmanagedState]

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

  def isoState[Tags2, ManagedState2, UnmanagedState2](isoT: Tags <=> Tags2, isoS: S <=> PState[Tags2, ManagedState2, UnmanagedState2])(implicit mf: Monad[F]) =
    iso0[Tags2, ManagedState2, UnmanagedState2, F](isoT, isoS, isoNaturalRefl)

  def isoState[ManagedState2, UnmanagedState2](isoS: S <=> PState[Tags, ManagedState2, UnmanagedState2])(implicit mf: Monad[F]) =
    iso0[Tags, ManagedState2, UnmanagedState2, F](isoRefl, isoS, isoNaturalRefl)

  def isoState[ManagedState2, UnmanagedState2](toS2: S => PState[Tags, ManagedState2, UnmanagedState2], fromS2: PState[Tags, ManagedState2, UnmanagedState2] => S)(implicit mf: Monad[F]) =
    iso0[Tags, ManagedState2, UnmanagedState2, F](isoRefl, new (S <=> PState[Tags, ManagedState2, UnmanagedState2]) {
      def to = toS2
      def from = fromS2
    }, isoNaturalRefl)

  def isoUnmanagedState[UnmanagedState2](toS2: UnmanagedState => UnmanagedState2, fromS2: UnmanagedState2 => UnmanagedState)(implicit mf: Monad[F]) =
    iso0[Tags, ManagedState, UnmanagedState2, F](isoRefl, new (S <=> PState[Tags, ManagedState, UnmanagedState2]) {
      def to = (s: S) => PState[Tags, ManagedState, UnmanagedState2](managed = s.managed, unmanaged = toS2(s.unmanaged))
      def from = (s2: PState[Tags, ManagedState, UnmanagedState2]) => PState(managed = s2.managed, unmanaged = fromS2(s2.unmanaged))
    }, isoNaturalRefl)

  def iso0[Tags2, ManagedState2, UnmanagedState2, F2[_]](
    isoT: Tags <=> Tags2, isoS: S <=> PState[Tags2, ManagedState2, UnmanagedState2], isoF: F <~> F2
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
        }
      }
    }
  }

  trait ResumeStep[A, T]
  case class FlatMapStep[A, T](v: F[(Precepte[A], S, T)]) extends ResumeStep[A, T]
  case class ReturnStep[A, T](v: (S, A, T)) extends ResumeStep[A, T]

  sealed trait Precepte[A] {
    self =>

    final def flatMap[B](f: A => Precepte[B]): Precepte[B] =
      Flatmap[A, B](() => self, f)

    final def map[B](f: A => B): Precepte[B] =
      flatMap(a => Return(f(a)))

    def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Precepte[AP[A]] =
      this.map(a => ap.point(a))

    @tailrec private final def resume[T](state: S, t: T, z: (S, T) => T)(implicit fu: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): ResumeStep[A, T] = this match {
      case Return(a) =>
        ReturnStep((state, a, t))

      case Step(st, tags) =>
        val state0 = upd.appendTags(state, tags)
        // append tags to managed state and propagate this new managed state to next step
        FlatMapStep(st.run(state0).map { case (s, p) => (p, s, z(s, t)) })

      case Flatmap(sub, next) =>
        sub() match {
          case Return(a) =>
            next(a).resume(state, t, z)

          case Step(st, tags) =>
            val state0 = upd.appendTags(state, tags)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            // Do not reuse appended segment but original state
            FlatMapStep(st.run(state0).map { case (s, p) =>
              val s1 = upd.updateUnmanaged(state, s.unmanaged)
              (p.flatMap(next), s1, z(s, t))
            })

          case f@Flatmap(sub2, next2) =>
            (Flatmap(sub2, (z:f._I) => next2(z).flatMap(next)):Precepte[A]).resume(state, t, z)
        }
    }

    final def scan[T](state: S, t: T, z: (S, T) => T)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[(S, A, T)] =
      this.resume(state, t, z) match {
        case FlatMapStep(fsp) =>
          fsp.flatMap { case (p0, s0, t0) => p0.scan(s0, t0, z) }
        case ReturnStep(sat) =>
          sat.point[F]
      }


    final def run(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[(S, A)] =
      scan[Unit](state, (), (_, _) => ()).map{ case (s, a, t) => (s, a) }

    final def eval(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[A] =
      run(state).map(_._2)

    final def observe(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[(S, A, Vector[S])] =
      scan(state, Vector.empty[S], (s, t) => t :+ s)

    // TODO: tailrec
    final def mapStep(f: StepState ~> StepState)(implicit F: Functor[F]): Precepte[A] =
      this match {
        case Return(a) =>
          Return(a)
        case Step(st, tags) =>
          Step(f(st).map(_.mapStep(f)), tags)
        case fl@Flatmap(sub, next) =>
          Flatmap(() => sub().mapStep(f), (n: fl._I) => next(n).mapStep(f))
      }
  }

  case class Return[A](a: A) extends Precepte[A]

  case class Step[A](st: StepState[A], tags: Tags) extends Precepte[A]

  case class Flatmap[I, A](sub: () => Precepte[I], next: I => Precepte[A]) extends Precepte[A] {
    type _I = I
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
          ???
      }

  }

  object Precepte extends LowPriorityManagedStatetances {

    trait PrecepteBuilder {
      val tags: Tags

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