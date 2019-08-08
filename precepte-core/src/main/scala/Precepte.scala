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

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

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

  final type precepte[A0] = Precepte[T, M, U, F, A0]

  @inline final def flatMap[B](f: A => precepte[B]): precepte[B] =
    Precepte.flatMap[T, M, U, F, A, B](this)(f)

  @inline final def map[B](f: A => B): precepte[B] =
    Precepte.map(this)(f)

  @inline final def ap[B](f: precepte[A => B]): precepte[B] =
    Precepte.ap(this)(f)

  @inline final def recoverWith(
      handler: Throwable => precepte[A]): precepte[A] =
    Precepte.catchError(this)(handler)

  @inline final def attempt: precepte[Try[A]] =
    map(x => Success(x): Try[A]).recoverWith { e =>
      Precepte.pure[T, M, U, F, Try[A]](Failure(e))
    }

  @inline final def lift[G[_]](
      implicit ap: MetaApplicative[G]): precepte[G[A]] =
    map(a => ap.pure(a))
  @inline final def interpret[G[_]](nt: F ~~> G)(
      implicit G: MetaMonadPrecepteEffect[T, M, U, G],
      upd: PStateUpdater[T, M, U]): G[A] = Precepte.interpret(nt)(this)

  @inline final def runAs[G[_]](nt: F ~~> G)(state: PState[T, M, U])(
      implicit mo: MetaMonad[G],
      FE: MetaErrorEffect[Throwable, G],
      trampolinedF: MetaDefer[G],
      upd: PStateUpdater[T, M, U],
      S: MetaSemigroup[U]): G[(PState[T, M, U], A)] =
    Precepte.run[T, M, U, F, A, G](nt)(this)(state)

  @inline final def run(state: PState[T, M, U])(
      implicit mo: MetaMonad[F],
      FE: MetaErrorEffect[Throwable, F],
      trampolinedF: MetaDefer[F],
      upd: PStateUpdater[T, M, U],
      S: MetaSemigroup[U]): F[(PState[T, M, U], A)] =
    Precepte.run(~~>.id[F])(this)(state)

  @inline final def eval(state: PState[T, M, U])(
      implicit mo: MetaMonad[F],
      FE: MetaErrorEffect[Throwable, F],
      upd: PStateUpdater[T, M, U],
      trampolinedF: MetaDefer[F],
      S: MetaSemigroup[U]): F[A] =
    mo.map(run(state))(_._2)

  @inline final def graph(g0: Graph)(
      implicit nod: ToNode[PState[T, M, U]]): precepte[(Graph, A)] =
    Precepte.graph(g0)(this)

  def mapSuspension(f: SubStepInstrumentation[T, M, U, F]): precepte[A]

  /** translates your effect into another one using a natural transformation */
  @inline final def compile[G[_]](iso: F <~~> G): Precepte[T, M, U, G, A] =
    Precepte.iso(iso)(this)

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
      f: SubStepInstrumentation[T, M, U, F]): Pure[T, M, U, F, A] = this
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
      f: SubStepInstrumentation[T, M, U, F]): Lift[T, M, U, F, A] = this
}

/** Trampolining
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  */
private[precepte] final case class Defer[T, M, U, F[_], A](
    deferred: () => Precepte[T, M, U, F, A])
    extends Precepte[T, M, U, F, A] {
  @inline final def mapSuspension(
      f: SubStepInstrumentation[T, M, U, F]): Precepte[T, M, U, F, A] =
    Defer(() => deferred().mapSuspension(f))
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
  def mapSuspension(
      f: SubStepInstrumentation[T, M, U, F]): GetPState[T, M, U, F] = this
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
  def mapSuspension(
      f: SubStepInstrumentation[T, M, U, F]): SetPState[T, M, U, F] = this
}

/** Precepte Raise Error
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  * @tparam A Output type (this value is inchanged!)
  */
private[precepte] final case class RaiseError[T, M, U, F[_], A](
    error: Throwable
) extends Precepte[T, M, U, F, A] {
  def mapSuspension(
      f: SubStepInstrumentation[T, M, U, F]): RaiseError[T, M, U, F, A] =
    this
}

/** Precepte Catch Error
  *
  * @tparam T Tags
  * @tparam M Managed State
  * @tparam U Unmanaged State
  * @tparam F Base functor
  * @tparam A Output type (this value is inchanged!)
  */
private[precepte] final case class CatchError[T, M, U, F[_], A](
    sub: Precepte[T, M, U, F, A],
    handler: Throwable => Precepte[T, M, U, F, A]
) extends Precepte[T, M, U, F, A] {
  def mapSuspension(
      f: SubStepInstrumentation[T, M, U, F]): CatchError[T, M, U, F, A] =
    CatchError(sub.mapSuspension(f),
               (e: Throwable) => handler(e).mapSuspension(f))
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
    nats: Vector[SubStepInstrumentation[T, M, U, F]],
    leaf: Boolean
) extends Precepte[T, M, U, F, A] {

  @inline final def mapSuspension(
      f: SubStepInstrumentation[T, M, U, F]): SubStep[T, M, U, F, A] =
    SubStep(sub.mapSuspension(f), tags, nats.:+(f), leaf)

  @inline final def intrumented: Precepte[T, M, U, F, A] =
    nats.foldLeft(sub) { case (acc, nat) => nat(acc) }
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
      f: SubStepInstrumentation[T, M, U, F]): Mapped[T, M, U, F, I, A] =
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
      f: SubStepInstrumentation[T, M, U, F]): Flatmap[T, M, U, F, I, A] =
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
      f: SubStepInstrumentation[T, M, U, F]): Apply[T, M, U, F, A, B] =
    Apply(pa.mapSuspension(f), pf.mapSuspension(f))
}

object Precepte {
  def apply[Ta](_tags: Ta): PrecepteBuilder[Ta] =
    new PrecepteBuilder[Ta] {
      val tags = _tags
    }

  //////////////////////////////////////////////////////////////////
  //  Precepte Public API

  /** Applicative Pure
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  def pure[T, M, U, F[_], A](a: A): Precepte[T, M, U, F, A] = Pure(a)

  /** Lift an F
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  def liftF[T, M, U, F[_], A](fa: F[A]): Precepte[T, M, U, F, A] =
    Lift(fa)

  /** Get the current State
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline def get[T, M, U, F[_]]: Precepte[T, M, U, F, PState[T, M, U]] =
    GetPState()

  /** Set the current State
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline def set[T, M, U, F[_]](
      state: PState[T, M, U]): Precepte[T, M, U, F, Unit] =
    SetPState[T, M, U, F](state)

  /** Modify the current State
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline def modify[T, M, U, F[_], A](
      f: PState[T, M, U] => (PState[T, M, U], A)): Precepte[T, M, U, F, A] =
    get[T, M, U, F].flatMap[A] { (s0: PState[T, M, U]) =>
      val (s1, a) = f(s0)
      set(s1).map(_ => a)
    }

  /** Raise an error
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline def raiseError[T, M, U, F[_], A](
      error: Throwable): Precepte[T, M, U, F, A] =
    RaiseError(error)

  /** Applicative ap
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline def catchError[T, M, U, F[_], A](sub: Precepte[T, M, U, F, A])(
      handler: Throwable => Precepte[T, M, U, F, A]): Precepte[T, M, U, F, A] =
    sub match {
      case Pure(_)       => sub
      case RaiseError(e) => handler(e)
      case _             => CatchError(sub, handler)
    }

  /** Delay the evaluation of the argument until this precepte is run.
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline def delay[T, M, U, F[_], A](deferred: => A): Precepte[T, M, U, F, A] =
    Defer(() => Pure(deferred))

  /** Delay the evaluation of the argument until this precepte is run.
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline def defer[T, M, U, F[_], A](
      deferred: => Precepte[T, M, U, F, A]): Precepte[T, M, U, F, A] =
    Defer(() => deferred)

  /** Delay the evaluation of the argument until this precepte is run.
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline final def deferredLift[T, M, U, F[_], A](
      ga: => F[A]): Precepte[T, M, U, F, A] =
    Defer(() => Lift(ga))

  /** Precepte Effect
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline final def subStep[T, M, U, F[_], A](
      tags: T,
      leaf: Boolean = true,
      nats: Vector[SubStepInstrumentation[T, M, U, F]] = Vector.empty
  )(
      sub: Precepte[T, M, U, F, A]
  ): Precepte[T, M, U, F, A] = SubStep(sub, tags, nats, leaf)

  /** Functorial map
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline def map[T, M, U, F[_], A, B](pa: Precepte[T, M, U, F, A])(
      f: A => B): Precepte[T, M, U, F, B] =
    pa match {
      case Pure(a) => Pure(f(a))
      case _       => Mapped(pa, f)
    }

  /** Monadic FlatMap
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline def flatMap[T, M, U, F[_], A, B](pa: Precepte[T, M, U, F, A])(
      f: A => Precepte[T, M, U, F, B]): Precepte[T, M, U, F, B] =
    pa match {
      case Pure(a) => f(a)
      case _       => Flatmap(pa, f)
    }

  /** Applicative ap
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline def ap[T, M, U, F[_], A, B](pa: Precepte[T, M, U, F, A])(
      pf: Precepte[T, M, U, F, A => B]): Precepte[T, M, U, F, B] =
    (pa, pf) match {
      case (Pure(a), Pure(f)) => Pure(f(a))
      case (_, _)             => Apply(pa, pf)
    }

  ///////////////////////////////////////////////////
  // Functions

  /** Conversion from Try to Precepte
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  @inline def fromTry[T, M, U, F[_], A](v: Try[A]): Precepte[T, M, U, F, A] =
    v match {
      case Success(a) => pure(a)
      case Failure(e) => raiseError(e)
    }

  /** Print the Graph
    *
    * @tparam T Tags
    * @tparam M Managed State
    * @tparam U Unmanaged State
    * @tparam F Base functor
    */
  final def graph[T, M, U, F[_], A](g0: Graph)(p: Precepte[T, M, U, F, A])(
      implicit nod: ToNode[PState[T, M, U]]): Precepte[T, M, U, F, (Graph, A)] =
    p match {
      case Pure(a) =>
        Pure(g0 -> a)

      case RaiseError(e) =>
        RaiseError(e)

      case CatchError(sub, h) =>
        CatchError(sub.graph(g0), (e: Throwable) => h(e).graph(g0))

      case _: GetPState[T, M, U, F] =>
        Mapped(GetPState(), (s: PState[T, M, U]) => g0 -> s)

      case set: SetPState[T, M, U, F] =>
        Mapped(set, (_: Unit) => (g0, ()))

      case Lift(fa) =>
        Lift(fa).map(g0 -> _)

      case Defer(deferred) =>
        Defer(() => deferred().graph(g0))

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
  final def iso[T, M, U, F[_], A, G[_]](isoFG: F <~~> G)(
      p: Precepte[T, M, U, F, A]): Precepte[T, M, U, G, A] =
    p match {
      case Pure(a)                    => Pure(a)
      case _: GetPState[T, M, U, F]   => GetPState()
      case set: SetPState[T, M, U, F] => SetPState(set.state)
      case Lift(fa)                   => Lift(isoFG.to(fa))
      case Defer(deferred) =>
        Defer(() => deferred().compile(isoFG))
      case RaiseError(e) => RaiseError(e)

      case CatchError(sub, h) =>
        CatchError(iso(isoFG)(sub), (e: Throwable) => iso(isoFG)(h(e)))

      case SubStep(sub, tags, nats, leaf) =>
        SubStep(sub.compile(isoFG),
                tags,
                nats.map(SubStepInstrumentation.iso(isoFG)),
                leaf)

      case Mapped(sub2, f2) => Mapped(sub2.compile(isoFG), f2)
      case f @ Flatmap(sub, next) =>
        def next2(gi: f._I): Precepte[T, M, U, G, A] =
          next(gi).compile(isoFG)
        Flatmap(sub.compile(isoFG), next2)

      case Apply(pa, pfa) =>
        Apply(pa.compile(isoFG), pfa.compile(isoFG))
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

      case Defer(deferred) =>
        Defer(() => deferred().xmapState(to, from))

      case RaiseError(e) =>
        RaiseError(e)

      case CatchError(sub, h) =>
        CatchError(xmapState(to, from)(sub),
                   (e: Throwable) => xmapState(to, from)(h(e)))

      case SubStep(sub, tags, nats, leaf) =>
        SubStep(
          sub.xmapState(to, from),
          tags,
          nats.map(SubStepInstrumentation.xmapUnmanagedState(to, from)),
          leaf
        )

      case Mapped(sub2, f2) => Mapped(sub2.xmapState(to, from), f2)

      case fm @ Flatmap(sub, next) =>
        def next2(gi: fm._I) = next(gi).xmapState(to, from)
        Flatmap(sub.xmapState(to, from), next2)

      case Apply(pa, pfa) =>
        Apply(pa.xmapState(to, from), pfa.xmapState(to, from))
    }

  @inline final def interpret[T, M, U, F[_], A, G[_]](nt: F ~~> G)(
      p: Precepte[T, M, U, F, A])(
      implicit G: MetaMonadPrecepteEffect[T, M, U, G],
      upd: PStateUpdater[T, M, U]): G[A] = {
    type P[Z] = Precepte[T, M, U, F, Z]

    def aux[X](px: P[X]): G[X] =
      px match {
        case Pure(x)       => G.pure(x)
        case Lift(gx)      => nt(gx)
        case RaiseError(e) => G.raiseError(e)
        case CatchError(sub, h) =>
          G.catchError(G.defer(aux(sub))) { (e: Throwable) =>
            G.defer(aux(h(e)))
          }
        case Defer(deferred) =>
          val p: P[X] =
            try { deferred() } catch { case NonFatal(e) => RaiseError(e) }
          aux(p)

        case Mapped(sub, next) => G.map(G.defer(aux(sub)))(next)
        case Flatmap(sub, next) =>
          G.flatMap(G.defer(aux(sub))) { r =>
            G.defer(aux(next(r)))
          }
        case Apply(pa, pf) =>
          G.ap(G.defer(aux(pa)))(G.defer(aux(pf)))
        case _: GetPState[T, M, U, F]   => G.get
        case set: SetPState[T, M, U, F] => G.set(set.state)

        case sub: SubStep[T, M, U, F, a] =>
          G.flatMap(G.modify(upd.appendTags(_, sub.tags))) { _ =>
            G.defer(aux(sub.intrumented))
          }

      }
    aux(p)
  }

  @inline final def run[T, M, U, F[_], A, G[_]](nt: F ~~> G)(
      p: Precepte[T, M, U, F, A])(st9999: PState[T, M, U])(
      implicit G: MetaMonad[G],
      GE: MetaErrorEffect[Throwable, G],
      GT: MetaDefer[G],
      upd: PStateUpdater[T, M, U],
      U: MetaSemigroup[U]): G[(PState[T, M, U], A)] = {

    type P[Z] = Precepte[T, M, U, F, Z]
    type State = PState[T, M, U]
    type GS[X] = G[(State, X)]

    @inline def auxTrickTailRec[X, B](st0: State,
                                      px: P[X],
                                      cont: Cont[X, B]): GS[B] =
      aux(st0, px, cont)

    sealed abstract class Cont[X, B]
    final case class IdK[B]() extends Cont[B, B]
    final case class MappedK[I, X, B](next: I => X, cont: Cont[X, B])
        extends Cont[I, B]
    final case class FlatmapK[I, X, B](next: I => P[X], cont: Cont[X, B])
        extends Cont[I, B]
    final case class ApplyK1[I, X, B](originalState: State,
                                      secondState: State,
                                      next: P[I => X],
                                      cont: Cont[X, B])
        extends Cont[I, B]
    final case class ApplyK2[I, X, B](originalState: State,
                                      res1: GS[I],
                                      cont: Cont[X, B])
        extends Cont[I => X, B]
    final case class CatchErrorK[X, B](st0: State,
                                       handler: Throwable => P[X],
                                       cont: Cont[X, B])
        extends Cont[X, B]

    final case class InvokeAux[X, B](st0: State, px: P[X], cont: Cont[X, B])

    @tailrec
    def aux[X, B](st0: State, px: P[X], cont: Cont[X, B]): GS[B] = {

      type Ret = Either[GS[B], InvokeAux[_, B]]

      @tailrec
      def runCont[C](cont: Cont[C, B], gx: G[(State, C)]): Ret =
        cont match {
          case _: IdK[a] =>
            Left[GS[B], InvokeAux[_, B]](gx)
          case MappedK(next, k) =>
            runCont(k, G.map(gx) { case (st1, i) => (st1, next(i)) })
          case fmk: FlatmapK[i, x, a] =>
            runCont[x](fmk.cont, G.flatMap(gx) {
              case (s1, i) =>
                GT.defer(auxTrickTailRec[x, x](s1, fmk.next(i), IdK()))
            })

          case apk: ApplyK1[i, x, a] =>
            Right(
              InvokeAux[i => x, a](
                apk.secondState,
                apk.next,
                ApplyK2[i, x, a](apk.originalState, gx, apk.cont)))

          case apk: ApplyK2[i, x, a] =>
            runCont[x](
              apk.cont,
              G.ap[(State, i), (State, x)](apk.res1)(
                G.map[(State, i => x), ((State, i)) => (State, x)](gx) {
                  case (stf, g) => {
                    case (sta, a) =>
                      val st1 =
                        upd.updateUnmanaged(
                          upd.recombine(apk.originalState, sta, stf),
                          U.combine(sta.unmanaged, stf.unmanaged))
                      st1 -> g(a)
                  }
                })
            )

          case cek: CatchErrorK[x, b] =>
            runCont[x](cek.cont, GE.catchError[(State, x)](gx) { e: Throwable =>
              GT.defer(auxTrickTailRec[x, x](cek.st0, cek.handler(e), IdK()))
            })
        }

      px match {
        case Pure(x) =>
          runCont(cont, G.pure(st0 -> x)) match {
            case Left(g)                   => g
            case Right(i: InvokeAux[a, b]) => aux(i.st0, i.px, i.cont)
          }
        case Lift(gx) =>
          runCont(cont, G.map(nt(gx))(a => st0 -> a)) match {
            case Left(g)                   => g
            case Right(i: InvokeAux[a, b]) => aux(i.st0, i.px, i.cont)
          }
        case Defer(deferred) =>
          val p: P[X] =
            try { deferred() } catch { case NonFatal(e) => RaiseError(e) }
          aux(st0, p, cont)

        case Mapped(sub, next) =>
          aux(st0, sub, MappedK(next, cont))

        case Flatmap(sub, next) =>
          aux(st0, sub, FlatmapK(next, cont))

        case Apply(pa, pf) =>
          val (st1, st2) = upd.spawn(st0)
          aux(st1, pa, ApplyK1(st0, st2, pf, cont))

        case _: GetPState[T, M, U, F] =>
          runCont(cont, G.pure(st0 -> st0)) match {
            case Left(g)                   => g
            case Right(i: InvokeAux[a, b]) => aux(i.st0, i.px, i.cont)
          }
        case set: SetPState[T, M, U, F] =>
          runCont(cont, G.pure((set.state, ()))) match {
            case Left(g)                   => g
            case Right(i: InvokeAux[a, b]) => aux(i.st0, i.px, i.cont)
          }

        case RaiseError(e) =>
          runCont(cont, GE.raiseError[(State, X)](e)) match {
            case Left(g)                   => g
            case Right(i: InvokeAux[a, b]) => aux(i.st0, i.px, i.cont)
          }

        case CatchError(sub, h) =>
          aux(st0, sub, CatchErrorK(st0, h, cont))

        case sub: SubStep[T, M, U, F, X] =>
          val st1 = upd.appendTags(st0, sub.tags)
          aux(st1, sub.intrumented, cont)
      }
    }

    aux(st9999, p, IdK())
  }
  @inline def updateUnmamaged[T, M, U, F[_], A](x: (U, A))(
      implicit upd: PStateUpdater[T, M, U]): Precepte[T, M, U, F, A] =
    Precepte.modify { s =>
      (upd.updateUnmanaged(s, x._1), x._2)
    }

  trait PrecepteInstances[T, M, U, F[_]]
      extends MetaMonadPrecepteEffect[T, M, U, Precepte[T, M, U, F, ?]] {

    final type precepte[A] = Precepte[T, M, U, F, A]
    final type instrumentStep = SubStepInstrumentation[T, M, U, F]
    final type state = PState[T, M, U]

    @inline final def pure[A](x: A): precepte[A] =
      Precepte.pure[T, M, U, F, A](x)
    @inline final def lift[A](fa: F[A]): precepte[A] =
      Precepte.liftF[T, M, U, F, A](fa)
    @inline final def defer[A](ga: => precepte[A]): precepte[A] =
      Precepte.defer[T, M, U, F, A](ga)

    @inline final def deferredLift[A](ga: => F[A]): precepte[A] =
      Precepte.deferredLift[T, M, U, F, A](ga)

    @inline final def get: precepte[PState[T, M, U]] = Precepte.get[T, M, U, F]
    @inline final def set(s: PState[T, M, U]): precepte[Unit] =
      Precepte.set[T, M, U, F](s)

    @inline final def raiseError[A](e: Throwable): precepte[A] =
      Precepte.raiseError(e)

    @inline final def catchError[A](sub: precepte[A])(
        handler: Throwable => precepte[A]): precepte[A] =
      Precepte.catchError(sub)(handler)

    @inline final def subStep[A](
        tags: T,
        nats: Vector[SubStepInstrumentation[T, M, U, F]] = Vector.empty,
        leaf: Boolean = true)(
        sub: precepte[A]
    ): precepte[A] = Precepte.subStep(tags, leaf, nats)(sub)

    @inline final def map[A, B](fa: precepte[A])(f: A => B): precepte[B] =
      fa.map(f)
    @inline final def flatMap[A, B](fa: precepte[A])(
        f: A => precepte[B]): precepte[B] =
      fa.flatMap(f)
    @inline final def ap[A, B](fa: precepte[A])(
        f: precepte[A => B]): precepte[B] = fa.ap(f)

    @inline def fromTry[A](v: Try[A]): precepte[A] =
      Precepte.fromTry(v)
  }

  implicit def precepteInstances[T, M, U, F[_]]: PrecepteInstances[T, M, U, F] =
    new PrecepteInstances[T, M, U, F] {}

  final type API[T, M, U, F[_]] = PrecepteInstances[T, M, U, F]
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
