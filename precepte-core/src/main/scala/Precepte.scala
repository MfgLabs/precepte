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
  * @tparam T, the type representing a Tag for a step
  * @tparam M, the type representing the managed part of the state
  * @tparam U, the type representing the unmanaged part of the state
  * @tparam F, the type of the effects wrapped in this Precepte
  * @tparam A, the type of the value returned by te Precepte
  */
sealed trait Precepte[T, M, U, F[_], A] {
  self =>

  final type PX[A0] = Precepte[T, M, U, F, A0]

  @inline final def flatMap[B](
      f: A => Precepte[T, M, U, F, B]): Precepte[T, M, U, F, B] =
    Flatmap[T, M, U, F, A, B](self, f)

  @inline final def map[B](f: A => B): Precepte[T, M, U, F, B] =
    Mapped(this, f)

  @inline final def ap[B](
      f: Precepte[T, M, U, F, A => B]): Precepte[T, M, U, F, B] =
    Apply(this, f)

  final def lift[AP[_]](
      implicit ap: MetaApplicative[AP]): Precepte[T, M, U, F, AP[A]] =
    map(a => ap.pure(a))

  final def run(state: PState[T, M, U], idx: Int = 0, maxDepth: Int = 100)(
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
          case Pure(a) => f(a)

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
        case Pure(a) =>
          ReturnStep(a, stateResume)

        case GetPState() =>
          ReturnStep(stateResume, stateResume)

        case set: SetPState[T, M, U, F] =>
          ReturnStep((), set.state)

        case Lift(fa) =>
          FStep(mo.map(fa)(a => stateResume -> a))

        case Defer(defered) =>
          resume(idxResume)(stateResume)(defered())

        case Apply(pa, pf) =>
          ApplyStep(pa, pf, (x: X) => Pure(x))

        case substep: SubStep[T, M, U, F, X] =>
          // append tags to managed state and propagate this new managed state to next step
          val state0 = upd.appendTags(stateResume, substep.tags, idxResume)
          FStep(substep.intrumented.run(state0))

        case mf: Mapped[T, M, U, F, i, X] =>
          mf.sub match {
            case sub: SubStep[T, M, U, F, i] =>
              // append tags to managed state and propagate this new managed state to next step
              val state0 = upd.appendTags(stateResume, sub.tags, idxResume)
              FStep(mo.map(sub.intrumented.run(state0)) {
                case (s1, a1) => s1 -> mf.next(a1)
              })

            case Pure(a) =>
              ReturnStep(mf.next(a), stateResume)

            case Defer(defered) =>
              resume(idxResume)(stateResume)(Mapped(defered(), mf.next))

            case Lift(fa) =>
              FStep(mo.map(fa)(a => stateResume -> mf.next(a)))

            case GetPState() =>
              ReturnStep(mf.next(stateResume), stateResume)

            case set: SetPState[T, M, U, F] =>
              ReturnStep(mf.next(()), set.state)

            case Mapped(sub2, pf2) =>
              MapFusionStep(sub2, pf2, mf.next, stateResume)

            case Flatmap(sub2, next2) =>
              resume(idxResume)(stateResume)(fastFlatMap(sub2)(z =>
                next2(z).map(mf.next)))

            case Apply(pa, pfa) =>
              ApplyStep(pa, pfa, (i: mf._I) => Pure(mf.next(i)))
          }

        case fm: Flatmap[T, M, U, F, i, X] =>
          fm.sub match {
            case Pure(a) =>
              resume(idxResume)(stateResume)(fm.next(a))

            case GetPState() =>
              resume(idxResume)(stateResume)(fm.next(stateResume))

            case set: SetPState[T, M, U, F] =>
              resume(idxResume)(set.state)(fm.next(()))

            case Defer(defered) =>
              resume(idxResume)(stateResume)(Flatmap(defered(), fm.next))

            case sub: SubStep[T, M, U, F, i] =>
              // append tags to managed state and propagate this new managed state to next step
              val state0 = upd.appendTags(stateResume, sub.tags, idxResume)
              // repass state as a Step in a Flatmap means the flatMap chain is finished
              FMStep(sub.intrumented.run(state0), fm.next)

            case Lift(fa) =>
              FMStep(mo.map(fa)(a => stateResume -> a), fm.next)

            case Mapped(sub2, f2) =>
              resume(idxResume)(stateResume)(fastFlatMap(sub2)(z =>
                fm.next(f2(z))))

            case Flatmap(sub2, next2) =>
              resume(idxResume)(stateResume)(fastFlatMap(sub2)(z =>
                fastFlatMap(next2(z))(fm.next)))

            case Apply(pa, pfa) =>
              ApplyStep(pa, pfa, fm.next)
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

  final def eval(state: PState[T, M, U])(implicit mo: MetaMonad[F],
                                         upd: PStateUpdater[T, M, U],
                                         S: MetaSemigroup[U]): F[A] =
    mo.map(run(state))(_._2)

  @inline final def graph(g0: Graph)(
      implicit nod: ToNode[PState[T, M, U]]): Precepte[T, M, U, F, (Graph, A)] =
    Precepte.graph(g0)(this)

  def mapSuspension(f: InstrumentStep[T, M, U, F]): Precepte[T, M, U, F, A]

  /** translates your effect into another one using a natural transformation */
  @inline final def compile[G[_]](iso: F <~~> G): Precepte[T, M, U, G, A] =
    Precepte.compile(iso)(this)

  @inline final def xmapState[UnmanagedState2](
      to: U => UnmanagedState2,
      from: UnmanagedState2 => U): Precepte[T, M, UnmanagedState2, F, A] =
    Precepte.xmapState(to, from)(this)
}

/** Applicative/Monadic Pure
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class Pure[T, M, U, F[_], A](a: A)
    extends Precepte[T, M, U, F, A] {
  @inline final def mapSuspension(
      f: InstrumentStep[T, M, U, F]): Pure[T, M, U, F, A] = this
}

/** Applicative/Monadic Lift
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class Lift[T, M, U, F[_], A](fa: F[A])
    extends Precepte[T, M, U, F, A] {
  @inline final def mapSuspension(
      f: InstrumentStep[T, M, U, F]): Lift[T, M, U, F, A] = this
}

/** Trampolining
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class Defer[T, M, U, F[_], A](
    defered: () => Precepte[T, M, U, F, A])
    extends Precepte[T, M, U, F, A] {
  @inline final def mapSuspension(
      f: InstrumentStep[T, M, U, F]): Precepte[T, M, U, F, A] =
    Defer(() => defered().mapSuspension(f))
}

/** Precepte Effect
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class GetPState[T, M, U, F[_]]()
    extends Precepte[T, M, U, F, PState[T, M, U]] {
  def mapSuspension(f: InstrumentStep[T, M, U, F]): GetPState[T, M, U, F] = this
}

/** Precepte Effect
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  * @tparam A Output type (this value is inchanged!)
  */
private[precepte] final case class SetPState[T, M, U, F[_]](
    state: PState[T, M, U]
) extends Precepte[T, M, U, F, Unit] {
  def mapSuspension(f: InstrumentStep[T, M, U, F]): SetPState[T, M, U, F] = this
}

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

  @inline final def mapSuspension(
      f: InstrumentStep[T, M, U, F]): Mapped[T, M, U, F, I, A] =
    Mapped(sub.mapSuspension(f), next)
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

  @inline final def mapSuspension(
      f: InstrumentStep[T, M, U, F]): Flatmap[T, M, U, F, I, A] =
    Flatmap(sub.mapSuspension(f), (i: I) => next(i).mapSuspension(f))
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

  @inline final def mapSuspension(
      f: InstrumentStep[T, M, U, F]): Apply[T, M, U, F, A, B] =
    Apply(pa.mapSuspension(f), pf.mapSuspension(f))
}

/** Precepte Effect
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class SubStep[T, M, U, F[_], A](
    sub: Precepte[T, M, U, F, A],
    tags: T,
    nats: Vector[InstrumentStep[T, M, U, F]],
    leaf: Boolean
) extends Precepte[T, M, U, F, A] {

  @inline final def mapSuspension(
      f: InstrumentStep[T, M, U, F]): SubStep[T, M, U, F, A] =
    SubStep(sub.mapSuspension(f), tags, nats.:+(f), leaf)

  @inline final def intrumented: Precepte[T, M, U, F, A] =
    nats.foldLeft(sub) { case (acc, nat) => nat(acc) }
}

object Precepte extends Implicits {
  def apply[Ta](_tags: Ta): PrecepteBuilder[Ta] =
    new PrecepteBuilder[Ta] {
      val tags = _tags
    }

  def liftF[Ta, M, U, F[_], A](fa: F[A]): Precepte[Ta, M, U, F, A] =
    Lift(fa)

  def pure[Ta, M, U, F[_], A](a: A): Precepte[Ta, M, U, F, A] = Pure(a)

  @inline def get[T, M, U, F[_]]: Precepte[T, M, U, F, PState[T, M, U]] =
    GetPState()

  @inline def set[T, M, U, F[_]](
      state: PState[T, M, U]): Precepte[T, M, U, F, Unit] =
    SetPState[T, M, U, F](state)

  @inline def defer[T, M, U, F[_], A](
      defered: => Precepte[T, M, U, F, A]): Precepte[T, M, U, F, A] =
    Defer(() => defered)

  @inline final def deferedLift[T, M, U, F[_], A](
      ga: => F[A]): Precepte[T, M, U, F, A] =
    Defer(() => Lift(ga))

  @inline def delay[T, M, U, F[_], A](defered: => A): Precepte[T, M, U, F, A] =
    Defer(() => Pure(defered))

  @inline def updateUnmamaged[T, M, U, F[_], A](x: (U, A))(
      implicit upd: PStateUpdater[T, M, U]): Precepte[T, M, U, F, A] =
    for {
      state0 <- get
      _ <- set(upd.updateUnmanaged(state0, x._1))
    } yield x._2

  final def graph[T, M, U, F[_], A](g0: Graph)(p: Precepte[T, M, U, F, A])(
      implicit nod: ToNode[PState[T, M, U]]): Precepte[T, M, U, F, (Graph, A)] =
    p match {
      case Pure(a) =>
        Pure(g0 -> a)

      case _: GetPState[T, M, U, F] =>
        Mapped(GetPState(), (s: PState[T, M, U]) => g0 -> s)

      case set: SetPState[T, M, U, F] =>
        Mapped(set, (_: Unit) => (g0, ()))

      case Lift(fa) =>
        Lift(fa).map(g0 -> _)

      case Defer(defered) =>
        defered().graph(g0)

      case ps: SubStep[T, M, U, F, A] =>
        Flatmap[T, M, U, F, PState[T, M, U], (Graph, A)](
          GetPState(),
          (b: PState[T, M, U]) =>
            Mapped[T, M, U, F, (Graph, A), (Graph, A)](
              SubStep(
                ps.sub.graph(Graph.empty),
                ps.tags,
                ps.nats,
                ps.leaf
              ), {
                case (g, a) =>
                  val node = nod.toNode(b)
                  val leaf =
                    if (ps.leaf) Leaf(node.id, node.value)
                    else Sub(node.id, node.value, g)
                  (g0 + leaf, a)
              }
          )
        )

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

  /** translates your effect into another one using a natural transformation */
  final def compile[T, M, U, F[_], A, G[_]](iso: F <~~> G)(
      p: Precepte[T, M, U, F, A]): Precepte[T, M, U, G, A] =
    p match {

      case Pure(a) => Pure(a)

      case _: GetPState[T, M, U, F] => GetPState()

      case set: SetPState[T, M, U, F] => SetPState(set.state)

      case Lift(fa) => Lift(iso.to(fa))

      case Defer(defered) =>
        defered().compile(iso)

      case SubStep(sub, tags, nats, leaf) =>
        SubStep(sub.compile(iso),
                tags,
                nats.map(InstrumentStepFun.iso(iso)),
                leaf)

      case Mapped(sub2, f2) => Mapped(sub2.compile(iso), f2)

      case f @ Flatmap(sub, next) =>
        def next2(gi: f._I): Precepte[T, M, U, G, A] =
          next(gi).compile(iso)
        Flatmap(sub.compile(iso), next2)

      case Apply(pa, pfa) =>
        Apply(pa.compile(iso), pfa.compile(iso))
    }

  @inline final def xmapState[T, M, U, F[_], A, U2](to: U => U2, from: U2 => U)(
      p: Precepte[T, M, U, F, A]): Precepte[T, M, U2, F, A] =
    p match {
      case Pure(a) => Pure(a)

      case _: GetPState[T, M, U, F] =>
        Mapped(GetPState[T, M, U2, F](),
               (_: PState[T, M, U2]).mapUnmanaged(from))

      case set: SetPState[T, M, U, F] =>
        SetPState[T, M, U2, F](set.state.mapUnmanaged(to))

      case Lift(fa) => Lift(fa)

      case Defer(defered) =>
        defered().xmapState(to, from)

      case SubStep(sub, tags, nats, leaf) =>
        SubStep(
          sub.xmapState(to, from),
          tags,
          nats.map(InstrumentStepFun.xmapUnmanagedState(to, from)),
          leaf
        )

      case Mapped(sub2, f2) => Mapped(sub2.xmapState(to, from), f2)

      case fm @ Flatmap(sub, next) =>
        def next2(gi: fm._I) = next(gi).xmapState(to, from)
        Flatmap(sub.xmapState(to, from), next2)

      case Apply(pa, pfa) =>
        Apply(pa.xmapState(to, from), pfa.xmapState(to, from))
    }
  /*
  @inline final def run[T, M, U, F[_], G[_]](
      nt: F ~~> G,idx: Int = 0)(implicit G: MetaMonadPrecepte[T,M,U,G], upd: PStateUpdater[T,M,U]): Precepte[T,M,U,F,?] ~~> G = {
    type P[A] = Precepte[T, M, U, F, A]
    new (P ~~> G) {
      def apply[A](pa: P[A]): G[A] =
        pa match {
          case Pure(x)                  => G.pure(x)
          case Lift(gx)                 => G.defer(nt(gx))
          case Mapped(sub, next)        => G.map(G.defer(apply(sub)))(next)
          case Flatmap(sub, next)       => G.flatMap(G.defer(apply(sub))) { r => G.defer(apply(next(r))) }
          case Apply(pa, pf)            => G.ap(G.defer(apply(pa)))(G.defer(apply(pf)))
          case GetPState()              => G.get
          case SetPState(s)             => G.set(s)
          case SubStep(sub, tags, nats, leaf) =>
            G.flatMap(G.get) { state0 =>
              // append tags to managed state and propagate this new managed state to next step
              val updatedState = upd.appendTags(state0, tags, 0)

              G.flatMap(G.set(updatedState)) { _ =>
                val gsub =
                val instrumented =
                  nats.foldLeft(gsub) {
                    case (acc, nat) => nat(state0 -> acc)
                  }

                FStep(finalst)
              }
            }
        }
    }
  }*/
}

trait PrecepteBuilder[Ta] {
  val tags: Ta

  // Suspends an effect in the context of tagged step
  // By construction, the returned Precepte is necessarily a Map (coyoneda trick)
  final def apply[M, U, F[_], A](
      f: PState[Ta, M, U] => F[A]): Precepte[Ta, M, U, F, A] =
    SubStep[Ta, M, U, F, A](
      Flatmap(GetPState(), (s: PState[Ta, M, U]) => Lift[Ta, M, U, F, A](f(s))),
      tags,
      Vector.empty,
      true
    )

  // Suspends an effect and updates the unmanaged state in the context of tagged step
  // By construction, the returned Precepte is necessarily a Map (coyoneda trick)
  final def applyU[M, U, F[_], A](f: PState[Ta, M, U] => F[(U, A)])(
      implicit upd: PStateUpdater[Ta, M, U]): Precepte[Ta, M, U, F, A] =
    Flatmap[Ta, M, U, F, (U, A), A](
      SubStep[Ta, M, U, F, (U, A)](
        Flatmap(GetPState(), (st0: PState[Ta, M, U]) => Lift(f(st0))),
        tags,
        Vector.empty,
        true
      ),
      Precepte.updateUnmamaged(_)
    )

  // Suspends an effect and updates the unmanaged state in the context of tagged step
  // By construction, the returned Precepte is necessarily a Map (coyoneda trick)
  final def applyP[M, U, F[_], A](
      f: PState[Ta, M, U] => Precepte[Ta, M, U, F, A])
    : Precepte[Ta, M, U, F, A] =
    SubStep(Flatmap(GetPState(), f), tags, Vector.empty, false)

  // Suspends an effect and updates the unmanaged state in the context of tagged step
  // By construction, the returned Precepte is necessarily a Map (coyoneda trick)
  final def applyPU[M, U, F[_], A](
      f: PState[Ta, M, U] => Precepte[Ta, M, U, F, (U, A)])(
      implicit upd: PStateUpdater[Ta, M, U]): Precepte[Ta, M, U, F, A] =
    Flatmap(
      SubStep[Ta, M, U, F, (U, A)](
        Flatmap(GetPState(), f),
        tags,
        Vector.empty,
        false
      ),
      Precepte.updateUnmamaged(_: (U, A))
    )

  // Suspends a Precepte in the concept of a Step
  // The other coyoneda trick
  def apply[M, U, F[_], A](
      m: => Precepte[Ta, M, U, F, A]): Precepte[Ta, M, U, F, A] =
    applyP(_ => m)
}

trait PrecepteAPI[T, M, U, F[_]]
    extends MetaMonadPrecepte[T, M, U, Precepte[T, M, U, F, ?]] {

  final type precepte[A] = Precepte[T, M, U, F, A]
  final type instrumentStep = InstrumentStep[T, M, U, F]
  final type state = PState[T, M, U]

  @inline final def pure[A](x: A): precepte[A] = Precepte.pure[T, M, U, F, A](x)
  @inline final def lift[A](fa: F[A]): precepte[A] =
    Precepte.liftF[T, M, U, F, A](fa)
  @inline final def defer[A](ga: => precepte[A]): precepte[A] =
    Precepte.defer[T, M, U, F, A](ga)

  @inline final def deferedLift[A](ga: => F[A]): precepte[A] =
    Precepte.deferedLift[T, M, U, F, A](ga)

  @inline final def get: precepte[PState[T, M, U]] = Precepte.get[T, M, U, F]
  @inline final def set(s: PState[T, M, U]): precepte[Unit] =
    Precepte.set[T, M, U, F](s)
  @inline final def map[A, B](fa: precepte[A])(f: A => B): precepte[B] =
    fa.map(f)
  @inline final def flatMap[A, B](fa: precepte[A])(
      f: A => precepte[B]): precepte[B] =
    fa.flatMap(f)
  @inline final def ap[A, B](fa: precepte[A])(
      f: precepte[A => B]): precepte[B] = fa.ap(f)
}

trait Implicits {

  @inline implicit def precepteMetaMonad[Ta, ManagedState, UnmanagedState, F[_]]
    : MetaMonad[Precepte[Ta, ManagedState, UnmanagedState, F, ?]] =
    new MetaMonad[Precepte[Ta, ManagedState, UnmanagedState, F, ?]] {
      override def pure[A](
          a: A): Precepte[Ta, ManagedState, UnmanagedState, F, A] =
        Pure(a)
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
