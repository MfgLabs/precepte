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
import scala.annotation.tailrec

/**
  * Precepte, let your code be state-aware and make runtime effect observation acceptable...
  *
  * Precepte is an opinionated purely functional & lazy API stacking some useful Metamonads to help you observe the execution of your runtime effects
  * by instrumenting your code with a managed state propagated all along your business workflow.
  *
  * Precepte embraces the concept that observing has a cost but let you control explicitly the balance between precision and performance without
  * sacrifying code cleanness and FP purity & laziness.
  *
  * Precepte's idea is simply to make your code _state-aware_ by enabling you to obtain a State anywhere you need in your code.
  * This state will provide you with:
  *   - a context defining from where this code comes and where it is (function name, custom tags, etc..)
  *   - any values you want to measure in your observation
  *
  * The state provided by Precepte is composed of:
  *   - a managed part that is managed by Precepte and that consists in:
  *       * the Span: a global ID uniquely identifying the full execution workflow
  *       * the Call Path: a sequence of tuples of PId (local step ID) and Tags (defined at compile-time)
  *                        accumulated by Precepte all along the execution of the workflow. This is what
  *                         provides you with the place from where you come and where you are...
  *
  *   - an unmanaged part that is just a container in which you can put anything you want and also perform some compile-time DI
  *
  *
  * {{{
  *  // A Sample with Future effects (the ugliest of the effects)
  *  // For now, we use scalaz (cats coming soon) so you need some Metamonads from Scalaz
  *  import scala.concurrent.ExecutionContext.Implicits.global
  *  import scala.concurrent.Future
  *  import scalaz.std.scalaFuture._
  *  import scalaz.syntax.Metamonad._
  *
  *  // import the default Precepte representation using our provided Tags & ManagedState type
  *  import precepte.default._
  *
  *  // create some effectful steps in which you can
  *  def f1 = Precepte(tags("simple.f1")){(_: ST[Unit]) => 1.point[Future]}
  *  def f2(i: Int) = Precepte(tags("simple.f2")){(_: ST[Unit]) => s"foo \$i".point[Future]}
  *
  *  // Lazy definition of your effectful workflow
  *  val res = for {
  *    i <- f1
  *    r <- f2(i)
  *  } yield r
  *
  *  // create the initial state
  *  val state0 = ST(Span.gen, env, Vector.empty, ())
  *
  *  // execute your effectful workflow starting with state0
  *  val (s, a) = res.run(state0)
  * }}}
  *
  * Precepte is a custom purely functional structure based on:
  *   - A State MetaMonad to represent the pure propagation of the State in the workflow
  *   - A Free MetaMonad to represent our Metamonadic & Metaapplicative effectful workflow
  *   - Some helpers to make Precepte usable with MetaMonad Transformers
  *   - Coyoneda to reduce the requirement of effects to be Functor in Free MetaMonads
  *   - some custom optimization to reduce the burden of Free MetaMonads classic model (right associatio, map fusion, structure squashing, )
  *
  * ```
  *
  * ```
  * This is the main ADT:
  *
  * - Ta, the type representing a Tag for a step
  * - ManagedState, the type representing the managed part of the state
  * - UnManageState, the type representing the unmanaged part of the state
  * - F, the type of the effects wrapped in this Precepte
  * - A, the type of the value returned by te Precepte
  */
sealed trait Precepte[Ta, ManagedState, UnmanagedState, F[_], A] {
  self =>

  type PX[A0] = Precepte[Ta, ManagedState, UnmanagedState, F, A0]

  type S = PState[Ta, ManagedState, UnmanagedState]

  final def flatMap[B](f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
    Flatmap[Ta, ManagedState, UnmanagedState, F, A, B](self, f)

  final def map[B](f: A => B): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
    SMap(this, f)

  def lift[AP[_]](implicit ap: MetaApplicative[AP]): Precepte[Ta, ManagedState, UnmanagedState, F, AP[A]] =
    map(a => ap.pure(a))

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
    (implicit fu: MetaMonad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: MetaSemigroup[UnmanagedState])
    : ResumeStep[Ta, ManagedState, UnmanagedState, F, A] = {

    def resumeSub[I, A0](s: SubStep[Ta, ManagedState, UnmanagedState, F, I, A0]) =
      s match {
        case ss@SubStep(sub, map, tags) =>
          def up(s: S) = upd.appendTags(s, tags, idx)
          val mf =
            (f: F[(S, I)]) => fu.map(f){ case (s, i) =>
              map(up(state), s, i)
            }
          val s0 = up(state) // use the updated state in the execution of the underlying function
          FXSStep(sub(s0), mf, up) // pass up so that the precepte returned by this function can also use the updated state
      }

    this match {

      case Return(a) =>
        ReturnStep(a, state)

      case Suspend(fa) =>
        FStep(fu.map(fa)(a => state -> a))

      case StepMap(fst, fmap, tags) =>
        // append tags to managed state and propagate this new managed state to next step
        val state0 = upd.appendTags(state, tags, idx)
        FStep(fu.map(fst(state0))( a => fmap(state0, a) ))

      case a@Apply(pa, pf) =>
        ApplyStep(pa, pf)

      case sub@SubStep(_, _, _) =>
        resumeSub(sub)

      case mf@SMap(sub, pf) =>
        sub match {
          case sub@SubStep(_, _, _) =>
            val r = resumeSub(sub)
            NextStep(r, (i: mf._I) => Return(pf(i)))

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
            val n: mf._I => Precepte[Ta, ManagedState, UnmanagedState, F, A] = i => Return(pf(i))
            val ap = ApplyStep(pa, pfa)
            NextStep(ap, n)
        }

      case f@Flatmap(sub, next) =>
        sub match {
          case Return(a) =>
            next(a).resume(idx)(state)

          case sub@SubStep(_, _, _) =>
            val r = resumeSub(sub)
            NextStep(r, next)

          case Suspend(fa) =>
            val fs = FStep(fu.map(fa)(a => state -> a))
            NextStep(fs, next)

          case StepMap(fst, fmap, tags) =>
            val state0 = upd.appendTags(state, tags, idx)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            val fs =
              FStep(fu.map(fst(state0)) { a =>
                val (s1, a1) = fmap(state0, a)
                s1 -> a1
              })
            NextStep(fs, next)

          case SMap(sub2, f2) =>
            sub2.fastFlatMap(z => next(f2(z))).resume(idx)(state)

          case f@Flatmap(sub2, next2) =>
            sub2.fastFlatMap(z => next2(z).fastFlatMap(next)).resume(idx)(state)

          case Apply(pa, pfa) =>
            val ap = ApplyStep(pa, pfa)
            NextStep(ap, next)
        }
      }
    }


    final def run0
      (state: S, idx: Int = 0, maxDepth: Int = 100)
      (implicit mo: MetaMonad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: MetaSemigroup[UnmanagedState])
      : F[(S, A)] = {

      def stepRunResume[B](
        r: ResumeStep[Ta, ManagedState, UnmanagedState, F, B],
        state: S,
        idx: Int = 0
      ): F[(S, B)] =
        r match {
          case ReturnStep(a, s) => mo.pure(s -> a)

          case FStep(fsa0) => fsa0

          case NextStep(step, next) =>
            val r = stepRunResume(step, state)
            mo.flatMap(r) { case(s, a) =>
              stepRun0(next(a), s)
            }

          case ApplyStep(pa, pf) =>
            mo.map2(
              stepRun0(pa, state, idx + 1),
              stepRun0(pf, state, idx + 2)
            ){
              case ((s0, a), (s1, f)) =>
                val s = upd.updateUnmanaged(s0, S.combine(s0.unmanaged, s1.unmanaged))
                (s, f(a))
            }

          case mfs@MapFusionStep(p0, f0, f1, s0) =>
            // @tailrec
            def fusionStep[C, I, D](p: PX[C], f0: C => I, f1: I => D, s: S, depth: Int): F[(S, D)] = {
              p.resume(0)(s) match {

                case ReturnStep(a, s) => mo.map(mo.pure(s -> a)){ case (s2, a2) => (s2, f1(f0(a2))) }

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

                case ApplyStep(pa, pf) =>
                  mo.map2(
                    stepRun0(pa, state, idx + 1),
                    stepRun0(pf, state, idx + 2)
                  ){
                    case ((s1, a), (s2, f)) =>
                      val s = upd.updateUnmanaged(s0, S.combine(s1.unmanaged, s2.unmanaged))
                      (s, (f andThen f0 andThen f1)(a))
                  }

                case fx@FXSStep(pi, mf, up) =>
                  throw new NotImplementedError("331")
                  // mo.map(mf(stepRun0(pi, s))) { case (s, c) =>
                  //   s -> (f0 andThen f1)(c)
                  // }

                case n@NextStep(step, next) =>
                  val f = (f0 andThen f1)
                  mo.map(stepRunResume(n, s0)) { case (s, a) =>
                    (s, f(a))
                  }
              }
            }

            // depth is already 2 as we are in a mapfusionstep
            fusionStep(p0, f0, f1, s0, 2)

          case fx@FXSStep(pi, mf, up) =>
            val state0 = up(state)
            mf(stepRun0(pi, state0))
        }

      def stepRun0[B](p: PX[B], state: S, idx: Int = 0): F[(S, B)] =
        stepRunResume(p.resume(idx)(state), state, idx)

      stepRun0(this, state, idx)
    }

    final def run(state: S)(implicit mo: MetaMonad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: MetaSemigroup[UnmanagedState]): F[(S, A)] =
      run0(state)

    final def eval(state: S)(implicit mo: MetaMonad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: MetaSemigroup[UnmanagedState]): F[A] =
      mo.map(run(state))(_._2)

    final def graph(g0: Graph)(implicit nod: ToNode[S]): Precepte[Ta, ManagedState, UnmanagedState, F, (Graph, A)] =
      this match {
        case Return(a) =>
          Return(g0 -> a)

        case Suspend(fa) =>
          Suspend(fa).map(g0 -> _)

        case ps@SubStep(sub, fmap, tags) =>
          SubStep(
            s => sub(s).graph(Graph.empty),
            (b, s, gi: (Graph, ps._I)) => {
              val (g, i) = gi
              val node = nod.toNode(b)
              val (s1, a) = fmap(b, s, i)
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

    type SF[T] = (S, F[T])
    final def mapSuspension(f: SF ~~> F): Precepte[Ta, ManagedState, UnmanagedState, F, A] = this match {
      case Return(a) =>
        Return(a)

      case Suspend(fa) =>
        Suspend(fa)

      case SubStep(sub, fmap, tags) =>
        val sub0 = (s: S) => sub(s).mapSuspension(f)
        SubStep(sub0, fmap, tags)

      case StepMap(fst, fmap, tags)  =>
        StepMap(s => f(s -> fst(s)), fmap, tags)

      case SMap(sub2, f2) =>
        SMap(sub2.mapSuspension(f), f2)

      case fl@Flatmap(sub, next) =>
        Flatmap(sub.mapSuspension(f), (i :fl._I) => next(i).mapSuspension(f))

      case Apply(pa, pfa) =>
        Apply(pa.mapSuspension(f), pfa.mapSuspension(f))
    }

    /** translates your effect into another one using a natural transformation */
    final def compile[G[_]](iso: F <~~> G): Precepte[Ta, ManagedState, UnmanagedState, G, A] = this match {
      case Return(a) => Return(a)

      case Suspend(fa) => Suspend(iso.to(fa))

      case ps@SubStep(sub, fmap, tags) =>
        SubStep(s => sub(s).compile(iso), fmap, tags)

      case sm@StepMap(fst, fmap, tags)  =>
        def fst2 = (s:S) => iso.to(fst(s))
        StepMap(fst2, fmap, tags)

      case SMap(sub2, f2) => SMap(sub2.compile(iso), f2)

      case f@Flatmap(sub, next) =>
        def next2(gi: f._I) = next(gi).compile(iso)
        Flatmap(sub.compile(iso), next2)

      case Apply(pa, pfa) =>
        Apply(pa.compile(iso), pfa.compile(iso))
    }

    final def xmapState[UnmanagedState2](to0: UnmanagedState => UnmanagedState2, from0: UnmanagedState): Precepte[Ta, ManagedState, UnmanagedState2, F, A] =
      xmapState[UnmanagedState2](to0, (_:UnmanagedState2) => from0)

    final def xmapState[UnmanagedState2](to: UnmanagedState => UnmanagedState2, from: UnmanagedState2 => UnmanagedState): Precepte[Ta, ManagedState, UnmanagedState2, F, A] = this match {
      case Return(a) => Return(a)

      case Suspend(fa) => Suspend(fa)

      case ps@SubStep(sub, fmap, tags) =>
        def fmap2 = { (b: Precepte[Ta, ManagedState, UnmanagedState2, F, A]#S, s: Precepte[Ta, ManagedState, UnmanagedState2, F, A]#S, i: ps._I) =>
          val (s2, i2) = fmap(b.mapUnmanaged(s => from(s)), s.mapUnmanaged(s => from(s)), i)
          (s2.mapUnmanaged(s => to(s)), i2)
        }
        SubStep(s => sub(s.mapUnmanaged(s => from(s))).xmapState(to, from), fmap2, tags)

      case sm@StepMap(fst, fmap, tags)  =>
        def fst2 = (s:Precepte[Ta, ManagedState, UnmanagedState2, F, A]#S) => fst(s.mapUnmanaged(s => from(s)))
        def fmap2 = { (s: Precepte[Ta, ManagedState, UnmanagedState2, F, A]#S, i: sm._I) =>
          val (s2, i2) = fmap(s.mapUnmanaged(s => from(s)), i)
          (s2.mapUnmanaged(s => to(s)), i2)
        }
        StepMap(fst2, fmap2, tags)

      case SMap(sub2, f2) => SMap(sub2.xmapState(to, from), f2)

      case fm@Flatmap(sub, next) =>
        def next2(gi: fm._I) = next(gi).xmapState(to, from)
        Flatmap(sub.xmapState(to, from), next2)

      case Apply(pa, pfa) =>
        Apply(pa.xmapState(to, from), pfa.xmapState(to, from))
    }
  }


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
  sub: Precepte[Ta, ManagedState, UnmanagedState, F, A]#S => Precepte[Ta, ManagedState, UnmanagedState, F, I]
, map: (Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, I) => (Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, A)
, tags: Ta
) extends Precepte[Ta, ManagedState, UnmanagedState, F, A] {
  type _I = I
}

object Precepte extends Implicits {

  def apply[Ta](_tags: Ta) =
    new PrecepteBuilder[Ta] {
      val tags = _tags
    }

  def liftF[Ta, M, U, F[_], A](fa: F[A]): Precepte[Ta, M, U, F, A] =
    Suspend(fa)

  def pure[Ta, M, U, F[_], A](a: A): Precepte[Ta, M, U, F, A] = Return(a)
}

/**
  * PRECEPTE DSL
  */

/** A Step followed by a Map (mixes Step + Coyoneda) */
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




trait PrecepteBuilder[Ta] {
  val tags: Ta

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

  // Suspends an effect and updates the unmanaged state in the context of tagged step
  // By construction, the returned Precepte is necessarily a Map (coyoneda trick)
  def applyP[M, U, F[_], A](λ: Precepte[Ta, M, U, F, A]#S => Precepte[Ta, M, U, F, A]): Precepte[Ta, M, U, F, A] =
    SubStep(λ, (b, s, i: A) => (s, i), tags)

  // Suspends a Precepte in the concept of a Step
  // The other coyoneda trick
  def apply[M, U, F[_], A](m: => Precepte[Ta, M, U, F, A]): Precepte[Ta, M, U, F, A] =
    applyP(s => m)

}

private [precepte] sealed trait ResumeStep[Ta, ManagedState, UnmanagedState, F[_], A]

private [precepte] case class NextStep[Ta, ManagedState, UnmanagedState, F[_], A, B](
  step: ResumeStep[Ta, ManagedState, UnmanagedState, F, A]
, next: A => Precepte[Ta, ManagedState, UnmanagedState, F, B]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, B]

private [precepte] case class ReturnStep[Ta, ManagedState, UnmanagedState, F[_], A](
  v: A
, state: Precepte[Ta, ManagedState, UnmanagedState, F, A]#S
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A]

private [precepte] case class FStep[Ta, ManagedState, UnmanagedState, F[_], A](
  v: F[(Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, A)]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A]

private [precepte] case class MapFusionStep[Ta, ManagedState, UnmanagedState, F[_], A, I, B](
  v: Precepte[Ta, ManagedState, UnmanagedState, F, A]
, f1: A => I
, f2: I => B
, state: Precepte[Ta, ManagedState, UnmanagedState, F, B]#S
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, B] {
  type _I = I
}

private [precepte] case class ApplyStep[Ta, ManagedState, UnmanagedState, F[_], A, B](
  pa: Precepte[Ta, ManagedState, UnmanagedState, F, A]
, pf: Precepte[Ta, ManagedState, UnmanagedState, F, A => B]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, B]

private [precepte] case class FXSStep[Ta, ManagedState, UnmanagedState, F[_], I, A](
  pi: Precepte[Ta, ManagedState, UnmanagedState, F, I]
, mf: F[(Precepte[Ta, ManagedState, UnmanagedState, F, I]#S, I)] => F[(Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, A)]
, up: Precepte[Ta, ManagedState, UnmanagedState, F, I]#S => Precepte[Ta, ManagedState, UnmanagedState, F, I]#S
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, A] {
  type _I = I
}


trait Implicits {

  implicit def precepteMetaMonad[Ta, ManagedState, UnmanagedState, F[_]] =
    new MetaMonad[({ type λ[α] = Precepte[Ta, ManagedState, UnmanagedState, F, α] })#λ] {
      override def pure[A](a: A): Precepte[Ta, ManagedState, UnmanagedState, F, A] =
        Return(a)
      override def map[A, B](m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(f: A => B): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.map(f)
      override def flatMap[A, B](m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.flatMap(f)

      override def ap[A, B](pa: Precepte[Ta, ManagedState, UnmanagedState, F, A])(pab: Precepte[Ta, ManagedState, UnmanagedState, F, A => B]): Precepte[Ta, ManagedState, UnmanagedState, F, B] = {
        Apply(pa, pab)
      }
    }

}