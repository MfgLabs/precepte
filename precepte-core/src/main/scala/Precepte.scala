/*
Copyright 2015 Mfg labs.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{ Monad, Applicative, Functor, \/, \/-, -\/, IndexedStateT, StateT, Semigroup }
import scalaz.syntax.monad._

import scala.annotation.tailrec

sealed trait Precepte[Ta, ManagedState, UnmanagedState, F[_], A] {
  self =>

  type PX[A0] = Precepte[Ta, ManagedState, UnmanagedState, F, A0]

  type S = PState[Ta, ManagedState, UnmanagedState]

  final def flatMap[B](f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
    Flatmap[Ta, ManagedState, UnmanagedState, F, A, B](self, f)

  final def map[B](f: A => B): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
    SMap(this, f)

  def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Precepte[Ta, ManagedState, UnmanagedState, F, AP[A]] =
    map(a => ap.point(a))

  /** A flatMap that doesn't create more FlatMap levels
    * or
    * Not tail rec
    */
  private def fastFlatMap[B](f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B], maxDepth: Int = 500): Precepte[Ta, ManagedState, UnmanagedState, F, B] = {
    def step[C, D](
      p: Precepte[Ta, ManagedState, UnmanagedState, F, C],
      f: C => Precepte[Ta, ManagedState, UnmanagedState, F, D],
      d: Int = 0
    ): Precepte[Ta, ManagedState, UnmanagedState, F, D] = p match {
      case Return(a) => f(a)

      case fm@SMap(sub, pf) =>
        if(d < maxDepth) {
          step(sub, (a: fm._I) => f(pf(a)), d + 1)
        } else {
          sub.flatMap(a => f(pf(a)))
        }

      case fm@Flatmap(sub, next) =>
        if(d < maxDepth) {
          step(sub, (a: fm._I) => step(next(a), f, d + 1) , d + 1)
        } else {
          sub.flatMap(a => next(a).flatMap(f))
        }

      case x => x.flatMap(f)
    }

    step(this, f)
  }

  @tailrec private [precepte] final def resume(idx: Int)(state: S)
    (implicit fu: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState])
    : ResumeStep[Ta, ManagedState, UnmanagedState, F, A] = {
    this match {

      case Return(a) =>
        ReturnStep(a, state)

      case Suspend(fa) =>
        FStep(fu.map(fa)(a => state -> a))

      case StepMap(fst, fmap, tags) =>
        // append tags to managed state and propagate this new managed state to next step
        val state0 = upd.appendTags(state, tags, idx)
        FStep(fu.map(fst(state0))( a => fmap(state0, a) ))

      case Apply(pa, pf) =>
        ApplyStep(pa, pf, (a: A) => Return(a))

      case sub@SubStep(_, _, _) =>
        sub.toStepMap.resume(idx)(state)

      case mf@SMap(sub, pf) =>
        sub match {
          case sub@SubStep(_, _, _) =>
            sub.toStepMap.map(pf).resume(idx)(state)

          case Return(a) =>
            ReturnStep(pf(a), state)

          case Suspend(fa) =>
            FStep(fu.map(fa)(a => state -> pf(a)))

          case StepMap(fst, fmap, tags) =>
            val state0 = upd.appendTags(state, tags, idx)
            FStep(fu.map(fst(state0)){ a =>
              val (s1, a1) = fmap(state0, a)
              s1 -> pf(a1)
            })

          case SMap(sub2, pf2) =>
            MapFusionStep(sub2, pf2, pf, state)

          case f@Flatmap(sub2, next2) =>
            sub2.fastFlatMap(z => next2(z).map(pf)).resume(idx)(state)

          case Apply(pa, pfa) =>
            ApplyStep(pa, pfa, (i: mf._I) => Return(pf(i)))
        }

      case f@Flatmap(sub, next) =>
        sub match {
          case Return(a) =>
            next(a).resume(idx)(state)

          case sub@SubStep(_, _, _) =>
            sub.toStepMap.flatMap(next).resume(idx)(state)

          case Suspend(fa) =>
            FMStep(fa.map(a => state -> a), next)

          case StepMap(fst, fmap, tags) =>
            val state0 = upd.appendTags(state, tags, idx)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            FMStep(fst(state0).map { a =>
              val (s1, a1) = fmap(state0, a)
              s1 -> a1
            }, next)

          case SMap(sub2, f2) =>
            sub2.fastFlatMap(z => next(f2(z))).resume(idx)(state)

          case f@Flatmap(sub2, next2) =>
            sub2.fastFlatMap(z => next2(z).fastFlatMap(next)).resume(idx)(state)

          case Apply(pa, pfa) =>
            ApplyStep(pa, pfa, next)
        }
      }
    }


    final def run0
      (state: S, idx: Int = 0, maxDepth: Int = 100)
      (implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState])
      : F[(S, A)] = {

      def stepRun0[B](p: PX[B], state: S, idx: Int = 0): F[(S, B)] = {
        p.resume(idx)(state) match {

          case ReturnStep(a, s) => mo.point(s -> a)

          case FStep(fsa0) => fsa0

          case FMStep(fa0, next0) => mo.bind(fa0){ case (s1, a1) =>
            stepRun0(next0(a1), s1)
          }

          case ApplyStep(pa, pf, next) =>
            mo.bind(mo.map((stepRun0(pa, state, idx + 1) |@| stepRun0(pf, state, idx + 2)).tupled){
              case ((s0, a), (s1, f)) =>
                val s = upd.updateUnmanaged(s0, S.append(s0.unmanaged, s1.unmanaged))
                (s, f(a))
            }){ case (s0, b) =>
              stepRun0(next(b), s0, idx)
            }

          case mfs@MapFusionStep(p0, f0, f1, s0) =>
            // @tailrec
            def fusionStep[C, I, D](p: PX[C], f0: C => I, f1: I => D, s: S, depth: Int): F[(S, D)] = {
              p.resume(0)(s) match {

                case ReturnStep(a, s) => mo.point(s -> a).map { case (s2, a2) => (s2, f1(f0(a2))) }

                case mm@MapFusionStep(p1, f2, f3, s1) =>
                  if(depth >= maxDepth) {
                    mo.map(stepRun0(p1, s1)){ case (s1, a1) =>
                      (s1, f1(f0(f3(f2(a1)))))
                    }
                  } else {
                    fusionStep(p1, f0.compose(f3).compose(f2), f1, s, depth + 2)
                  }

                case FStep(fsa0) => mo.map(fsa0){ case (s1, a1) =>
                  s1 -> f1(f0(a1))
                }

                case FMStep(fa0, next0) => mo.bind(fa0){ case (s1, a1) =>
                  stepRun0(next0(a1), s1).map { case (s2, a2) => s2 -> f1(f0(a2)) }
                }

                case ApplyStep(pa, pf, next) =>
                  (stepRun0(pa, state, idx + 1) |@| stepRun0(pf, state, idx + 2)).tupled.map {
                    case ((s1, a), (s2, f)) =>
                      val s = upd.updateUnmanaged(s0, S.append(s1.unmanaged, s2.unmanaged))
                      (s, f(a))
                  }.flatMap { case (s3, b) =>
                    stepRun0(next(b), s3, idx).map { case (s4, a4) => (s4, f1(f0(a4))) }
                  }
              }
            }

            // depth is already 2 as we are in a mapfusionstep
            fusionStep(p0, f0, f1, s0, 2)

        }
      }

      stepRun0(this, state, idx)
    }

    final def run(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[(S, A)] =
      run0(state)

    final def eval(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[A] =
      run(state).map(_._2)

    final def graph(g0: Graph)(implicit nod: ToNode[S]): Precepte[Ta, ManagedState, UnmanagedState, F, (Graph, A)] =
      this match {
        case Return(a) =>
          Return(g0 -> a)

        case Suspend(fa) =>
          Suspend(fa).map(g0 -> _)

        case ps@SubStep(sub, fmap, tags) =>
          SubStep(
            sub.graph(Graph.empty),
            (s, gi: (Graph, ps._I)) => {
              val (g, i) = gi
              val node = nod.toNode(s)
              val (s1, a) = fmap(s, i)
              (s1, (g0 + Sub(node.id, node.value, g), a))
            },
            tags)

        case sm@StepMap(fst, fmap, tags)  =>
          val fmap2 = (s: S, i: sm._I) => {
            val node = nod.toNode(s)
            val g = g0 + Leaf(node.id, node.value)
            val (s2, a) = fmap(s, i)
            (s2, (g, a))
          }
          StepMap(fst, fmap2, tags)

        case m@SMap(sub2, f2) =>
          def f3(gi: (Graph, m._I)) = (gi._1, f2(gi._2))
          SMap(sub2.graph(g0), f3 _)

        case f@Flatmap(sub, next) =>
          def next2(gi: (Graph, f._I)) = next(gi._2).graph(gi._1)
          Flatmap(sub.graph(g0), next2)


        case ap@Apply(pa, pfa) =>
          Apply(pa.graph(Graph.empty), pfa.graph(Graph.empty).map { case (g2, fab) =>
            def f(bg: (Graph, ap._A)) = {
              val g1 = bg._1
              val a = bg._2
              g0.addBranches(g1, g2) -> fab(a)
            }
            f _
          })
      }

  }


// private [precepte] sealed trait SimplePrecepte[Ta, ManagedState, UnmanagedState, F[_], A] extends Precepte[Ta, ManagedState, UnmanagedState, F, A]
private [precepte] case class Return[Ta, ManagedState, UnmanagedState, F[_], A](a: A) extends Precepte[Ta, ManagedState, UnmanagedState, F, A]
private [precepte] case class Suspend[Ta, ManagedState, UnmanagedState, F[_], A](a: F[A]) extends Precepte[Ta, ManagedState, UnmanagedState, F, A]

// a map. Renamed to SMap to avoid conflicts with collection.Map
private [precepte] case class SMap[Ta, ManagedState, UnmanagedState, F[_], I, A](
  sub: Precepte[Ta, ManagedState, UnmanagedState, F, I]
, next: I => A
) extends Precepte[Ta, ManagedState, UnmanagedState, F, A] {
  type _I = I
}

private [precepte] case class SubStep[Ta, ManagedState, UnmanagedState, F[_], I, A](
  sub: Precepte[Ta, ManagedState, UnmanagedState, F, I]
, map: (Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, I) => (Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, A)
, tags: Ta
) extends Precepte[Ta, ManagedState, UnmanagedState, F, A] {
  type _I = I

  @inline private[precepte] def toStepMap
    (implicit
      fu: Monad[F],
      upd: PStateUpdater[Ta, ManagedState, UnmanagedState],
      S: Semigroup[UnmanagedState]
    ): Precepte[Ta, ManagedState, UnmanagedState, F, A] = {
    // val SubStep(p, fmap, tags) = this
    StepMap[Ta, ManagedState, UnmanagedState, F, (S, I), A](
      sub.run _,
      (_, si) => {
        map(si._1, si._2.asInstanceOf[I])
      },
      tags)
  }
}

/* A Step followed by a Map (mixes Step + Coyoneda) */
private [precepte] case class StepMap[Ta, ManagedState, UnmanagedState, F[_], I, A](
  st: Precepte[Ta, ManagedState, UnmanagedState, F, A]#S => F[I]
, map: (Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, I) => (Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, A)
, tags: Ta
) extends Precepte[Ta, ManagedState, UnmanagedState, F, A] {
  type _I = I
}

private [precepte] case class Flatmap[Ta, ManagedState, UnmanagedState, F[_], I, A](
  sub: Precepte[Ta, ManagedState, UnmanagedState, F, I]
, next: I => Precepte[Ta, ManagedState, UnmanagedState, F, A]
) extends Precepte[Ta, ManagedState, UnmanagedState, F, A] {
  type _I = I
}

private [precepte] case class Apply[Ta, ManagedState, UnmanagedState, F[_], A, B](
  pa: Precepte[Ta, ManagedState, UnmanagedState, F, A]
, pf: Precepte[Ta, ManagedState, UnmanagedState, F, A => B]
) extends Precepte[Ta, ManagedState, UnmanagedState, F, B] {
  type _A = A
}

trait LowPriorityManagedStatetances {
  implicit def precepteMonadManagedStatetance[Ta, ManagedState, UnmanagedState, F[_]] =
    new Monad[({ type λ[α] = Precepte[Ta, ManagedState, UnmanagedState, F, α] })#λ] {
      override def point[A](a: => A): Precepte[Ta, ManagedState, UnmanagedState, F, A] =
        Return(a)
      override def map[A, B](m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(f: A => B): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.map(f)
      override def bind[A, B](m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.flatMap(f)

      override def ap[A, B](pa: => Precepte[Ta, ManagedState, UnmanagedState, F, A])(pab: => Precepte[Ta, ManagedState, UnmanagedState, F, A => B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] = {
        Apply(pa, pab)
      }
    }

}

  object Precepte extends LowPriorityManagedStatetances {

    trait PrecepteBuilder[Ta] {
      val tags: Ta

      import scala.concurrent.Future
      def future[M, U, A](λ: Precepte[Ta, M, U, Future, A]#S => Future[A])(implicit func: Functor[Future], ec: scala.concurrent.ExecutionContext): Precepte[Ta, M, U, Future, Throwable \/ A] =
        apply { pa =>
          func.map(λ(pa))(\/-.apply _)
            .recover{ case e => -\/(e) }
        }

      // Suspends an effect in the context of tagged step
      // By construction, the returned Precepte is necessarily a Map (coyoneda trick)
      def apply[M, U, F[_], A](λ: Precepte[Ta, M, U, F, A]#S => F[A]): Precepte[Ta, M, U, F, A] =
        StepMap[Ta, M, U, F, A, A](
          λ,
          { (st: Precepte[Ta, M, U, F, A]#S, a: A) => st -> a },
          tags
        )

      // Suspends an effect and updates the unmanaged state in the context of tagged step
      // By construction, the returned Precepte is necessarily a Map (coyoneda trick)
      def applyU[M, U, F[_], A](λ: Precepte[Ta, M, U, F, A]#S => F[(U, A)])(implicit upd: PStateUpdater[Ta, M, U]): Precepte[Ta, M, U, F, A] =
        StepMap[Ta, M, U, F, (U, A), A](
          λ,
          { (st: Precepte[Ta, M, U, F, A]#S, ua: (U, A)) =>
            val (unmanaged, a) = ua
            upd.updateUnmanaged(st, unmanaged) -> a
          },
          tags
        )

      // Suspends a Precepte in the concept of a Step
      // The other coyoneda trick
      def apply[M, U, F[_], A](m: => Precepte[Ta, M, U, F, A]): Precepte[Ta, M, U, F, A] =
        SubStep(m, (s, i: A) => (s, i), tags)

      def liftF[M, U, F[_], A](fa: F[A]): Precepte[Ta, M, U, F, A] =
        Suspend(fa)
    }

    def apply[Ta](_tags: Ta) =
      new PrecepteBuilder[Ta] {
        val tags = _tags
      }

  }

private [precepte] sealed trait ResumeStep[Ta, ManagedState, UnmanagedState, F[_], A]

private [precepte] case class ReturnStep[Ta, ManagedState, UnmanagedState, F[_], A](
  v: A
, state: Precepte[Ta, ManagedState, UnmanagedState, F, A]#S
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A]

private [precepte] case class FStep[Ta, ManagedState, UnmanagedState, F[_], A](
  v: F[(Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, A)]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A]

private [precepte] case class FMStep[Ta, ManagedState, UnmanagedState, F[_], I, A, B](
  v: F[(Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, A)]
, next: A => Precepte[Ta, ManagedState, UnmanagedState, F, B]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, B]

private [precepte] case class MapFusionStep[Ta, ManagedState, UnmanagedState, F[_], A, I, B](
  v: Precepte[Ta, ManagedState, UnmanagedState, F, A]
, f1: A => I
, f2: I => B
, state: Precepte[Ta, ManagedState, UnmanagedState, F, B]#S
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, B] {
  type _I = I
}

private [precepte] case class ApplyStep[Ta, ManagedState, UnmanagedState, F[_], A, B, C](
  pa: Precepte[Ta, ManagedState, UnmanagedState, F, A]
, pf: Precepte[Ta, ManagedState, UnmanagedState, F, A => B]
, next: B => Precepte[Ta, ManagedState, UnmanagedState, F, C]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, C]
