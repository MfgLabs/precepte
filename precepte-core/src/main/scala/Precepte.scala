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
  *  // import the default Precepte representation using our provided Tags & M type
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
  * - T, the type representing a Tag for a step
  * - M, the type representing the managed part of the state
  * - UnManageState, the type representing the unmanaged part of the state
  * - F, the type of the effects wrapped in this Precepte
  * - A, the type of the value returned by te Precepte
  */
sealed trait Precepte[T, M, U, F[_], A] {
  self =>

  final type PX[A0] = Precepte[T, M, U, F, A0]

  final def flatMap[B](
      f: A => Precepte[T, M, U, F, B]): Precepte[T, M, U, F, B] =
    Flatmap[T, M, U, F, A, B](self, f)

  final def map[B](f: A => B): Precepte[T, M, U, F, B] =
    Mapped(this, f)

  final def lift[AP[_]](
      implicit ap: MetaApplicative[AP]): Precepte[T, M, U, F, AP[A]] =
    map(a => ap.pure(a))

  final def run0(state: PState[T, M, U], idx: Int = 0, maxDepth: Int = 100)(
      implicit mo: MetaMonad[F],
      upd: PStateUpdater[T, M, U],
      S: MetaSemigroup[U]): F[(PState[T, M, U], A)] = {

    import cats.free.Trampoline
    import cats.instances.function._

    sealed trait ResumeStep[X]
    final case class ReturnStep[X](v: X, state: PState[T, M, U])
        extends ResumeStep[X]
    final case class FStep[X](v: F[(PState[T, M, U], X)]) extends ResumeStep[X]
    final case class FMStep[I, X](
        v: F[(PState[T, M, U], I)],
        next: I => Precepte[T, M, U, F, X]
    ) extends ResumeStep[X]

    final case class MapFusionStep[X, I, Y](
        v: Precepte[T, M, U, F, X],
        f1: X => I,
        f2: I => Y,
        state: PState[T, M, U]
    ) extends ResumeStep[Y] {
      type _I = I
    }

    final case class ApplyStep[X, Y, Z](
        pa: Precepte[T, M, U, F, X],
        pf: Precepte[T, M, U, F, X => Y],
        next: Y => Precepte[T, M, U, F, Z]
    ) extends ResumeStep[Z]

    @tailrec def resume[X](idxResume: Int)(stateResume: PState[T, M, U])(
        p: Precepte[T, M, U, F, X]): ResumeStep[X] = {

      /** A flatMap that doesn't create more FlatMap levels
        * or
        * Not tail rec
        */
      def fastFlatMap[Y, Z](p2: Precepte[T, M, U, F, Y])(
          f: Y => Precepte[T, M, U, F, Z],
          maxDepth: Int = 500): Precepte[T, M, U, F, Z] = {
        def step[C, D](
            p: Precepte[T, M, U, F, C],
            f: C => Precepte[T, M, U, F, D],
            d: Int = 0
        ): Precepte[T, M, U, F, D] = p match {
          case Return(a) => f(a)

          case fm @ Mapped(sub, pf) =>
            if (d < maxDepth) {
              step(sub, (a: fm._I) => f(pf(a)), d + 1)
            } else {
              sub.flatMap(a => f(pf(a)))
            }

          case fm @ Flatmap(sub, next) =>
            if (d < maxDepth) {
              step(sub, (a: fm._I) => step(next(a), f, d + 1), d + 1)
            } else {
              sub.flatMap(a => next(a).flatMap(f))
            }

          case x => x.flatMap(f)
        }

        step[Y, Z](p2, f)
      }

      p match {
        case Return(a) =>
          ReturnStep(a, stateResume)

        case Suspend(fa) =>
          FStep(mo.map(fa)(a => stateResume -> a))

        case StepMap(fst, fmap, tags) =>
          // append tags to managed state and propagate this new managed state to next step
          val state0 = upd.appendTags(stateResume, tags, idxResume)
          FStep(mo.map(fst(state0))(a => fmap(state0, a)))

        case Apply(pa, pf) =>
          ApplyStep(pa, pf, (x: X) => Return(x))

        case sub @ SubStep(_, _, _, _) =>
          resume(idxResume)(stateResume)(sub.toStepMap)

        case mf @ Mapped(sub, pf) =>
          sub match {
            case sub @ SubStep(_, _, _, _) =>
              resume(idxResume)(stateResume)(sub.toStepMap.map(pf))

            case Return(a) =>
              ReturnStep(pf(a), stateResume)

            case Suspend(fa) =>
              FStep(mo.map(fa)(a => stateResume -> pf(a)))

            case StepMap(fst, fmap, tags) =>
              val state0 = upd.appendTags(stateResume, tags, idxResume)
              FStep(mo.map(fst(state0)) { a =>
                val (s1, a1) = fmap(state0, a)
                s1 -> pf(a1)
              })

            case Mapped(sub2, pf2) =>
              MapFusionStep(sub2, pf2, pf, stateResume)

            case Flatmap(sub2, next2) =>
              resume(idxResume)(stateResume)(fastFlatMap(sub2)(z =>
                next2(z).map(pf)))

            case Apply(pa, pfa) =>
              ApplyStep(pa, pfa, (i: mf._I) => Return(pf(i)))
          }

        case Flatmap(sub, next) =>
          sub match {
            case Return(a) =>
              resume(idxResume)(stateResume)(next(a))

            case sub @ SubStep(_, _, _, _) =>
              resume(idxResume)(stateResume)(sub.toStepMap.flatMap(next))

            case Suspend(fa) =>
              FMStep(mo.map(fa)(a => stateResume -> a), next)

            case StepMap(fst, fmap, tags) =>
              val state0 = upd.appendTags(stateResume, tags, idxResume)
              // repass state as a Step in a Flatmap means the flatMap chain is finished
              FMStep(mo.map(fst(state0)) { a =>
                val (s1, a1) = fmap(state0, a)
                s1 -> a1
              }, next)

            case Mapped(sub2, f2) =>
              resume(idxResume)(stateResume)(fastFlatMap(sub2)(z =>
                next(f2(z))))

            case Flatmap(sub2, next2) =>
              resume(idxResume)(stateResume)(fastFlatMap(sub2)(z =>
                fastFlatMap(next2(z))(next)))

            case Apply(pa, pfa) =>
              ApplyStep(pa, pfa, next)
          }
      }
    }

    def stepRun0[B](p: PX[B],
                    state: PState[T, M, U],
                    idx: Int = 0): Trampoline[F[(PState[T, M, U], B)]] = {
      resume(idx)(state)(p) match {

        case ReturnStep(a, s) =>
          Trampoline.done(mo.pure(s -> a))

        case FStep(fsa0) =>
          Trampoline.done(fsa0)

        case FMStep(fa0, next0) =>
          Trampoline.done(mo.flatMap(fa0) {
            case (s1, a1) =>
              stepRun0(next0(a1), s1).run
          })

        case ApplyStep(pa, pf, next) =>
          for {
            sra <- stepRun0(pa, state, idx + 1)
            srf <- stepRun0(pf, state, idx + 2)
          } yield
            mo.flatMap(mo.map2(sra, srf) {
              case ((s0, a), (s1, f)) =>
                val s =
                  upd.updateUnmanaged(s0, S.combine(s0.unmanaged, s1.unmanaged))
                (s, f(a))
            }) {
              case (s0, b) =>
                stepRun0(next(b), s0, idx).run
            }

        case MapFusionStep(p0, f0, f1, s0) =>
          // @tailrec
          def fusionStep[C, I, D](
              p: PX[C],
              f0: C => I,
              f1: I => D,
              s: PState[T, M, U],
              depth: Int): Trampoline[F[(PState[T, M, U], D)]] = {
            resume(0)(s)(p) match {
              case ReturnStep(a, s) =>
                Trampoline.done(mo.map(mo.pure(s -> a)) {
                  case (s2, a2) => (s2, f1(f0(a2)))
                })

              case mfs: MapFusionStep[x, i, C] => //MapFusionStep(p1, f2, f3, s1) =>
                if (depth >= maxDepth) {
                  for {
                    sr1 <- stepRun0(mfs.v, mfs.state)
                  } yield
                    mo.map(sr1) {
                      case (s1, a1) =>
                        (s1, f1(f0(mfs.f2(mfs.f1(a1)))))
                    }
                } else
                  fusionStep(mfs.v,
                             f0.compose(mfs.f2).compose(mfs.f1),
                             f1,
                             s,
                             depth + 2)

              case FStep(fsa0) =>
                Trampoline.done(mo.map(fsa0) {
                  case (s1, a1) =>
                    s1 -> f1(f0(a1))
                })

              case FMStep(fa0, next0) =>
                Trampoline.done(mo.flatMap(fa0) {
                  case (s1, a1) =>
                    mo.map(stepRun0(next0(a1), s1).run) {
                      case (s2, a2) => s2 -> f1(f0(a2))
                    }
                })

              case ApplyStep(pa, pf, next) =>
                for {
                  sra <- stepRun0(pa, state, idx + 1)
                  srf <- stepRun0(pf, state, idx + 2)
                } yield
                  mo.flatMap(mo.map2(sra, srf) {
                    case ((s1, a), (s2, f)) =>
                      val s = upd.updateUnmanaged(s0,
                                                  S.combine(s1.unmanaged,
                                                            s2.unmanaged))
                      (s, f(a))
                  }) {
                    case (s3, b) =>
                      mo.map(stepRun0(next(b), s3, idx).run) {
                        case (s4, a4) => (s4, f1(f0(a4)))
                      }
                  }
            }
          }
          // depth is already 2 as we are in a mapfusionstep
          fusionStep(p0, f0, f1, s0, 2)
      }
    }

    stepRun0(this, state, idx).run
  }

  final def run(state: PState[T, M, U])(
      implicit mo: MetaMonad[F],
      upd: PStateUpdater[T, M, U],
      S: MetaSemigroup[U]): F[(PState[T, M, U], A)] =
    run0(state)

  final def eval(state: PState[T, M, U])(implicit mo: MetaMonad[F],
                                         upd: PStateUpdater[T, M, U],
                                         S: MetaSemigroup[U]): F[A] =
    mo.map(run(state))(_._2)

  final def graph(g0: Graph)(
      implicit nod: ToNode[PState[T, M, U]]): Precepte[T, M, U, F, (Graph, A)] =
    this match {
      case Return(a) =>
        Return(g0 -> a)

      case Suspend(fa) =>
        Suspend(fa).map(g0 -> _)

      case ps: SubStep[T, M, U, F, i, A] =>
        SubStep(
          s => ps.sub(s).graph(Graph.empty),
          (b, s, gi: (Graph, i)) => {
            val (g, i) = gi
            val node = nod.toNode(b)
            val (s1, a) = ps.fun(b, s, i)
            (s1, (g0 + Sub(node.id, node.value, g), a))
          },
          ps.tags,
          ps.nats
        )

      case sm @ StepMap(fst, fmap, tags) =>
        val fmap2 = (s: PState[T, M, U], i: sm._I) => {
          val node = nod.toNode(s)
          val g = g0 + Leaf(node.id, node.value)
          val (s2, a) = fmap(s, i)
          (s2, (g, a))
        }
        StepMap(fst, fmap2, tags)

      case m @ Mapped(sub2, f2) =>
        def f3(gi: (Graph, m._I)) = (gi._1, f2(gi._2))

        Mapped(sub2.graph(g0), f3 _)

      case f @ Flatmap(sub, next) =>
        def next2(gi: (Graph, f._I)) = next(gi._2).graph(gi._1)

        Flatmap(sub.graph(g0), next2)

      case ap @ Apply(pa, pfa) =>
        Apply(pa.graph(Graph.empty), pfa.graph(Graph.empty).map {
          case (g2, fab) =>
            def f(bg: (Graph, ap._A)) = {
              val g1 = bg._1
              val a = bg._2
              g0.addBranches(g1, g2) -> fab(a)
            }

            f _
        })
    }

  final def mapSuspension(
      f: InstrumentStep[T, M, U, F]): Precepte[T, M, U, F, A] = this match {
    case Return(a) =>
      Return(a)

    case Suspend(fa) =>
      Suspend(fa)

    case SubStep(sub, fmap, tags, nats) =>
      SubStep(s => sub(s).mapSuspension(f), fmap, tags, nats.:+(f))

    case StepMap(fst, fmap, tags) =>
      StepMap(s => f(s -> fst(s)), fmap, tags)

    case Mapped(sub2, f2) =>
      Mapped(sub2.mapSuspension(f), f2)

    case fl @ Flatmap(sub, next) =>
      Flatmap(sub.mapSuspension(f), (i: fl._I) => next(i).mapSuspension(f))

    case Apply(pa, pfa) =>
      Apply(pa.mapSuspension(f), pfa.mapSuspension(f))
  }

  /** translates your effect into another one using a natural transformation */
  final def compile[G[_]](iso: F <~~> G): Precepte[T, M, U, G, A] =
    this match {

      case Return(a) => Return(a)

      case Suspend(fa) => Suspend(iso.to(fa))

      case SubStep(sub, fmap, tags, nats) =>
        SubStep(s => sub(s).compile(iso),
                fmap,
                tags,
                nats.map(InstrumentStepFun.iso(iso)))

      case StepMap(fst, fmap, tags) =>
        def fst2(s: PState[T, M, U]) = iso.to(fst(s))
        StepMap(fst2, fmap, tags)

      case Mapped(sub2, f2) => Mapped(sub2.compile(iso), f2)

      case f @ Flatmap(sub, next) =>
        def next2(gi: f._I): Precepte[T, M, U, G, A] =
          next(gi).compile(iso)
        Flatmap(sub.compile(iso), next2)

      case Apply(pa, pfa) =>
        Apply(pa.compile(iso), pfa.compile(iso))
    }

  final def xmapState[UnmanagedState2](
      to0: U => UnmanagedState2,
      from0: U): Precepte[T, M, UnmanagedState2, F, A] =
    xmapState[UnmanagedState2](to0, (_: UnmanagedState2) => from0)

  final def xmapState[UnmanagedState2](
      to: U => UnmanagedState2,
      from: UnmanagedState2 => U): Precepte[T, M, UnmanagedState2, F, A] =
    this match {
      case Return(a) => Return(a)

      case Suspend(fa) => Suspend(fa)

      case ps @ SubStep(sub, fmap, tags, nats) =>
        def fmap2(b: PState[T, M, UnmanagedState2],
                  s: PState[T, M, UnmanagedState2],
                  i: ps._I): (PState[T, M, UnmanagedState2], A) = {
          val (s2, i2) =
            fmap(b.mapUnmanaged(s => from(s)), s.mapUnmanaged(s => from(s)), i)
          (s2.mapUnmanaged(s => to(s)), i2)
        }
        SubStep(
          s => sub(s.mapUnmanaged(s => from(s))).xmapState(to, from),
          fmap2,
          tags,
          nats.map(InstrumentStepFun.contraMapUnmanagedState(from))
        )

      case sm @ StepMap(fst, fmap, tags) =>
        def fst2(s: PState[T, M, UnmanagedState2]) =
          fst(s.mapUnmanaged(s => from(s)))
        def fmap2(s: PState[T, M, UnmanagedState2],
                  i: sm._I): (PState[T, M, UnmanagedState2], A) = {
          val (s2, i2) = fmap(s.mapUnmanaged(s => from(s)), i)
          (s2.mapUnmanaged(s => to(s)), i2)
        }
        StepMap(fst2, fmap2, tags)

      case Mapped(sub2, f2) => Mapped(sub2.xmapState(to, from), f2)

      case fm @ Flatmap(sub, next) =>
        def next2(gi: fm._I) = next(gi).xmapState(to, from)
        Flatmap(sub.xmapState(to, from), next2)

      case Apply(pa, pfa) =>
        Apply(pa.xmapState(to, from), pfa.xmapState(to, from))
    }
}

/** Applicative/Monadic Pure
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class Return[T, M, U, F[_], A](a: A)
    extends Precepte[T, M, U, F, A]

/** Applicative/Monadic Lift
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class Suspend[T, M, U, F[_], A](a: F[A])
    extends Precepte[T, M, U, F, A]

/** Functorial Map
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class Mapped[T, M, U, F[_], I, A](
    sub: Precepte[T, M, U, F, I],
    next: I => A
) extends Precepte[T, M, U, F, A] {
  type _I = I
}

/** Monadic FlatMap
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class Flatmap[T, M, U, F[_], I, A](
    sub: Precepte[T, M, U, F, I],
    next: I => Precepte[T, M, U, F, A]
) extends Precepte[T, M, U, F, A] {
  type _I = I
}

/** Monadic FlatMap
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class Apply[T, M, U, F[_], A, B](
    pa: Precepte[T, M, U, F, A],
    pf: Precepte[T, M, U, F, A => B]
) extends Precepte[T, M, U, F, B] {
  type _A = A
}

/** Precepte Effect
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class SubStep[T, M, U, F[_], I, A](
    sub: PState[T, M, U] => Precepte[T, M, U, F, I],
    fun: (PState[T, M, U], PState[T, M, U], I) => (PState[T, M, U], A),
    tags: T,
    nats: Vector[InstrumentStep[T, M, U, F]]
) extends Precepte[T, M, U, F, A] {
  type _I = I

  @inline private[precepte] def toStepMap(
      implicit
      fu: MetaMonad[F],
      upd: PStateUpdater[T, M, U],
      S: MetaSemigroup[U]): Precepte[T, M, U, F, A] = {
    val zero: Precepte[T, M, U, F, A] =
      StepMap[T, M, U, F, (PState[T, M, U], I), A](
                                                   // using same state twice ?
                                                   s => sub(s).run(s),
                                                   (b, si) => {
                                                     fun(b, si._1, si._2)
                                                   },
                                                   tags)

    nats.foldLeft(zero) { case (acc, nat) => acc.mapSuspension(nat) }
  }
}

/** A Step followed by a Map (mixes Step + Coyoneda)
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class StepMap[T, M, U, F[_], I, A](
    st: PState[T, M, U] => F[I],
    fun: (PState[T, M, U], I) => (PState[T, M, U], A),
    tags: T
) extends Precepte[T, M, U, F, A] {
  type _I = I
}

object Precepte extends Implicits {
  def apply[Ta](_tags: Ta): PrecepteBuilder[Ta] =
    new PrecepteBuilder[Ta] {
      val tags = _tags
    }

  def liftF[Ta, M, U, F[_], A](fa: F[A]): Precepte[Ta, M, U, F, A] =
    Suspend(fa)

  def pure[Ta, M, U, F[_], A](a: A): Precepte[Ta, M, U, F, A] = Return(a)
}

trait PrecepteBuilder[Ta] {
  val tags: Ta

  // Suspends an effect in the context of tagged step
  // By construction, the returned Precepte is necessarily a Map (coyoneda trick)
  final def apply[M, U, F[_], A](
      λ: PState[Ta, M, U] => F[A]): Precepte[Ta, M, U, F, A] =
    StepMap[Ta, M, U, F, A, A](
      λ, { (st: PState[Ta, M, U], a: A) =>
        st -> a
      },
      tags
    )

  // Suspends an effect and updates the unmanaged state in the context of tagged step
  // By construction, the returned Precepte is necessarily a Map (coyoneda trick)
  final def applyU[M, U, F[_], A](λ: PState[Ta, M, U] => F[(U, A)])(
      implicit upd: PStateUpdater[Ta, M, U]): Precepte[Ta, M, U, F, A] =
    StepMap[Ta, M, U, F, (U, A), A](
      λ, { (st: PState[Ta, M, U], ua: (U, A)) =>
        val (unmanaged, a) = ua
        upd.updateUnmanaged(st, unmanaged) -> a
      },
      tags
    )

  // Suspends an effect and updates the unmanaged state in the context of tagged step
  // By construction, the returned Precepte is necessarily a Map (coyoneda trick)
  final def applyP[M, U, F[_], A](
      λ: PState[Ta, M, U] => Precepte[Ta, M, U, F, A])
    : Precepte[Ta, M, U, F, A] =
    SubStep(λ, (_, s, i: A) => (s, i), tags, Vector.empty)

  // Suspends a Precepte in the concept of a Step
  // The other coyoneda trick
  def apply[M, U, F[_], A](
      m: => Precepte[Ta, M, U, F, A]): Precepte[Ta, M, U, F, A] =
    applyP(_ => m)

}

trait Implicits {

  @inline implicit def precepteMetaMonad[Ta, ManagedState, UnmanagedState, F[_]]
    : MetaMonad[Precepte[Ta, ManagedState, UnmanagedState, F, ?]] =
    new MetaMonad[Precepte[Ta, ManagedState, UnmanagedState, F, ?]] {
      override def pure[A](
          a: A): Precepte[Ta, ManagedState, UnmanagedState, F, A] =
        Return(a)
      override def map[A, B](
          m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(
          f: A => B): Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.map(f)
      override def flatMap[A, B](
          m: Precepte[Ta, ManagedState, UnmanagedState, F, A])(
          f: A => Precepte[Ta, ManagedState, UnmanagedState, F, B])
        : Precepte[Ta, ManagedState, UnmanagedState, F, B] =
        m.flatMap(f)

      override def ap[A, B](
          pa: Precepte[Ta, ManagedState, UnmanagedState, F, A])(
          pab: Precepte[Ta, ManagedState, UnmanagedState, F, A => B])
        : Precepte[Ta, ManagedState, UnmanagedState, F, B] = {
        Apply(pa, pab)
      }
    }
}
