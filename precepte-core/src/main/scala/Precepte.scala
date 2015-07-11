package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Bind, Monad, MonadPlus, Applicative, Functor, \/, \/-, -\/, IndexedStateT, StateT, State, Traverse, ~> }
import scalaz.Isomorphism.{<~>, <=>, isoRefl, isoNaturalRefl}
import scalaz.syntax.monad._

import scala.annotation.tailrec
import scalaz.{TreeLoc, Tree}


sealed trait Precepte[A] {
  self =>

  private trait ResumeStep[A, T]
  private case class FlatMapStep[A, T](v: F[(Precepte[A], S, T)]) extends ResumeStep[A, T]
  private case class ReturnStep[A, T](v: (S, A, T)) extends ResumeStep[A, T]

  type Tags
  type ManagedState
  type UnmanagedState
  type F[_]

  type S = PState[Tags, ManagedState, UnmanagedState]

  type Pre[B] = Precepte.Aux[Tags, ManagedState, UnmanagedState, F, B]

  final def flatMap[B](f: A => Pre[B]): Pre[B] =
    Flatmap[Tags, ManagedState, UnmanagedState, F, A, B](() => self, f)

  final def map[B](f: A => B): Pre[B] =
    ???
    // flatMap(a => Return(f(a)))

  def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Pre[AP[A]] =
    ???
    // this.map(a => ap.point(a))

  /*@tailrec*/ private final def resume[T](state: S, t: T, z: (S, T) => T)(implicit fu: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): ResumeStep[A, T] =
    ???
  // this match {
    // case Return(a) =>
    //   ReturnStep((state, a, t))

    // case Step(st, tags) =>
    //   val state0 = upd.appendTags(state, tags)
    //   // append tags to managed state and propagate this new managed state to next step
    //   FlatMapStep(st.run(state0).map { case (s, p) => (p, s, z(s, t)) })

    // case Flatmap(sub, next) =>
    //   sub() match {
    //     case Return(a) =>
    //       next(a).resume(state, t, z)

    //     case Step(st, tags) =>
    //       val state0 = upd.appendTags(state, tags)
    //       // repass state as a Step in a Flatmap means the flatMap chain is finished
    //       // Do not reuse appended segment but original state
    //       FlatMapStep(st.run(state0).map { case (s, p) =>
    //         val s1 = upd.updateUnmanaged(state, s.unmanaged)
    //         (p.flatMap(next), s1, z(s, t))
    //       })

    //     case f@Flatmap(sub2, next2) =>
    //       (Flatmap(sub2, (z:f._I) => next2(z).flatMap(next)):Precepte[A]).resume(state, t, z)
    //   }
  // }

  final def scan[T](state: S, t: T, z: (S, T) => T)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[(S, A, T)] =
    ???
    // this.resume(state, t, z) match {
    //   case FlatMapStep(fsp) =>
    //     fsp.flatMap { case (p0, s0, t0) => p0.scan(s0, t0, z) }
    //   case ReturnStep(sat) =>
    //     sat.point[F]
    // }


  final def run(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[(S, A)] =
    ???
    // scan[Unit](state, (), (_, _) => ()).map{ case (s, a, t) => (s, a) }

  final def eval(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[A] =
    ???
    // run(state).map(_._2)

  final def observe(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Tags, ManagedState, UnmanagedState]): F[(S, A, Vector[S])] =
    ???
    // scan(state, Vector.empty[S], (s, t) => t :+ s)

  // TODO: tailrec
  // final def mapStep(f: StepState ~> StepState)(implicit F: Functor[F]): Pre[A] =
  //   ???
    // this match {
    //   case Return(a) =>
    //     Return(a)
    //   case Step(st, tags) =>
    //     Step(f(st).map(_.mapStep(f)), tags)
    //   case fl@Flatmap(sub, next) =>
    //     Flatmap(() => sub().mapStep(f), (n: fl._I) => next(n).mapStep(f))
    // }
}

case class Return[T, M, U, F0[_], A](a: A) extends Precepte[A] {
  type Tags = T
  type ManagedState = M
  type UnmanagedState = U
  type F[T] = F0[T]
}

class Step[T, M, U, F0[_], A](st: StateT[F0, PState[T, M, U], Precepte.Aux[T, M, U, F0, A]], tags: Tags) extends Precepte[A] {
  type Tags = T
  type ManagedState = M
  type UnmanagedState = U
  type F[T] = F0[T]
}

case class Flatmap[T, M, U, F0[_], I, A](sub: () => Precepte.Aux[T, M, U, F0, I], next: I => Precepte.Aux[T, M, U, F0, A]) extends Precepte[A] {
  type _I = I

  type Tags = T
  type ManagedState = M
  type UnmanagedState = U
  type F[T] = F0[T]
}

trait LowPriorityManagedStatetances {
  implicit def precepteMonadManagedStatetance[T, M, U, F[_]] = {
    new Monad[({type λ[α] = Precepte.Aux[T, M, U, F, α]})#λ] {
      override def point[A](a: => A): Precepte.Aux[T, M, U, F, A] =
        Return[T, M, U, F, A](a)
      override def map[A, B](m: Precepte.Aux[T, M, U, F, A])(f: A => B): Precepte.Aux[T, M, U, F, B] =
        m.map(f)
      override def bind[A, B](m: Precepte.Aux[T, M, U, F, A])(f: A => Precepte.Aux[T, M, U, F, B]): Precepte.Aux[T, M, U, F, B] =
        m.flatMap(f)

      // override to support parallel execution
      // override def ap[A, B](pa: => Precepte.Aux[T, M, U, F, A])(pab: => Precepte.Aux[T, M, U, F, A => B]) =
      //   ???
    }
  }

}

object Precepte extends LowPriorityManagedStatetances {

  type Aux[T0, M0, U0, F0[_], A] = Precepte[A] {
    type Tags = T0
    type ManagedState = M0
    type UnmanagedState = U0
    type F[T] = F0[T]
  }

  /*
  trait PrecepteBuilder {
    val tags: Tags

    /**
    * Special constructor for Future
    */
    // import scala.concurrent.Future
    // def future[A](λ: S => F[A])(implicit F: Functor[F], ec: scala.concurrent.ExecutionContext, ev: F[A] =:= Future[A]): Precepte[Throwable \/ A] =
    //   apply { s =>
    //     λ(s)
    //       .map(a => \/.right[Throwable, A](a))
    //       .recover {
    //         case scala.util.control.NonFatal(e) =>
    //           \/.left[Throwable, A](e)
    //       }
    //   }

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
  */
}
