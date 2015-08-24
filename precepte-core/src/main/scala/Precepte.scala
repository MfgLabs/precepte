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
    Map[Ta, ManagedState, UnmanagedState, F, A, B](self, f)

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

      case fm@Map(sub, pf) =>
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
    (implicit mo: Functor[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState])
    : ResumeStep[Ta, ManagedState, UnmanagedState, F, A] = {
    this match {

      case Return(a) =>
        ReturnStep(a, state)

      case Suspend(fa) =>
        FStep(fa.map(a => state -> a))

      case StepMap(fst, fmap, tags) =>
        // append tags to managed state and propagate this new managed state to next step
        val state0 = upd.appendTags(state, tags, idx)
        FStep(fst(state0).map( a => fmap(state0, a) ))

      case Apply(pa, pf) =>
        ApplyStep(pa, pf, (a: A) => Return(a))

      case mf@Map(sub, pf) =>
        sub match {
          case Return(a) =>
            ReturnStep(pf(a), state)

          case Suspend(fa) =>
            FStep(fa.map(a => state -> pf(a)))
            // MapSuspendStep(fa, pf, state)

          case StepMap(fst, fmap, tags) =>
            val state0 = upd.appendTags(state, tags, idx)
            FStep(fst(state0).map { a =>
              val (s1, a1) = fmap(state0, a)
              s1 -> pf(a1)
            })

          case Map(sub2, pf2) =>
            // (Map(sub2, pf.compose(pf2)):Precepte[Ta, ManagedState, UnmanagedState, F, A]).resume(idx)(state)
            MapFusionStep(sub2, pf2, pf, state)

          case f@Flatmap(sub2, next2) =>
            sub2.fastFlatMap(z => next2(z).map(pf)).resume(idx)(state)
            // (Flatmap(sub2, (i: f._I) => next2(i).map(pf)):Precepte[Ta, ManagedState, UnmanagedState, F, A]).resume(idx)(state)

          case Apply(pa, pfa) =>
            ApplyStep(pa, pfa, (i: mf._I) => Return(pf(i)))
        }

      case f@Flatmap(sub, next) =>
        sub match {
          case Return(a) =>
            next(a).resume(idx)(state)

          case Suspend(fa) =>
            FMStep(fa.map(a => state -> a), next)

          case StepMap(fst, fmap, tags) =>
            val state0 = upd.appendTags(state, tags, idx)
            // repass state as a Step in a Flatmap means the flatMap chain is finished
            FMStep(fst(state0).map { a =>
              val (s1, a1) = fmap(state0, a)
              s1 -> a1
            }, next)

          case Map(sub2, f2) =>
            sub2.fastFlatMap(z => next(f2(z))).resume(idx)(state)
            // (Flatmap(sub2, (i: f._I) => next(f2(i))):Precepte[Ta, ManagedState, UnmanagedState, F, A]).resume(idx)(state)

          case f@Flatmap(sub2, next2) =>
            sub2.fastFlatMap(z => next2(z).fastFlatMap(next)).resume(idx)(state)
            // (Flatmap(sub2, (i: f._I) => next2(i).flatMap(next)):Precepte[Ta, ManagedState, UnmanagedState, F, A]).resume(idx)(state)

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

          case FMStep(fa0, next0) => fa0.flatMap { case (s1, a1) =>
            stepRun0(next0(a1), s1)
          }
          
          case ApplyStep(pa, pf, next) =>
            (stepRun0(pa, state, idx + 1) |@| stepRun0(pf, state, idx + 2)).tupled.map {
              case ((s0, a), (s1, f)) =>
                val s = upd.updateUnmanaged(s0, S.append(s0.unmanaged, s1.unmanaged))
                (s, f(a))
            }.flatMap { case (s0, b) =>
              stepRun0(next(b), s0, idx)
            }

          case mfs@MapFusionStep(p0, f0, f1, s0) =>
            @tailrec
            def fusionStep[C, I, D](p: PX[C], f0: C => I, f1: I => D, s: S, depth: Int): F[(S, D)] = {
              p.resume(0)(s) match {

                case ReturnStep(a, s) => mo.point(s -> a).map { case (s2, a2) => (s2, f1(f0(a2))) }

                case MapFusionStep(p1, f2, f3, s1) =>
                  if(depth >= maxDepth) {
                    stepRun0(p1, s1).map { case (s1, a1) =>
                      (s1, f1(f0(f3(f2(a1)))))
                    }
                  } else {
                    fusionStep(Map(p1, f3.compose(f2)), f0, f1, s1, depth + 2)
                  }

                case FStep(fsa0) => fsa0.map { case (s1, a1) =>
                  s1 -> f1(f0(a1))
                }

                case FMStep(fa0, next0) => fa0.flatMap { case (s1, a1) =>
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

/*
    final def scan[T]
      (append: (S, T) => T, merge: (T, T) => T, appendT: (T, T) => T, subG: (S, T) => T, isEmpty: T => Boolean, empty: T)
      (state: S, t: T, idx: Int = 0)
      (implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState])
      : F[(S, A, T)] = {
      def stepScan[B](p: PX[B], state: S, t: T, idx: Int = 0, isRoot: Boolean = false): F[(S, B, T)] = {
        p.resume(idx)(state) match {

          case SimpleStep(fsp) =>
            fsp.flatMap { case (p0, s0) =>

              stepScan(Return(p0), s0, empty).map { case (s1, a1, t1) =>

                val t2 = if(isEmpty(t1)) {
                  append(s0, t)
                } else {
                  if(!isRoot) {
                    appendT(t, subG(s0, t1))
                  } else {
                    appendT(append(s0, t), t1)
                  }
                }
                (s1, a1, t2)
              }
            }
/*
          case MapStep(fsp) =>
            fsp.flatMap { case (p0, f0, s0) =>
              stepScan(Return(p0), s0, empty).map { case (s1, a1, t1) =>
                val t2 = if(isEmpty(t1)) {
                  append(s0, t)
                } else {
                  if(!isRoot) {
                    appendT(t, subG(s0, t1))
                  } else {
                    appendT(append(s0, t), t1)
                  }
                }
                (s1, f0(a1), t2)
              }
            }

          case MapFusionStep((p0, f0, f1, s0)) =>
            stepScan(Return(p0), s0, empty).map { case (s1, a1, t1) =>
              val t2 = if(isEmpty(t1)) {
                append(s0, t)
              } else {
                if(!isRoot) {
                  appendT(t, subG(s0, t1))
                } else {
                  appendT(append(s0, t), t1)
                }
              }
              (s1, f1(f0(a1)), t2)
            }
*/
          case FlatMapStep(fsp) =>
            fsp.flatMap { case (a0, next0, s0) =>
              val t2 = if(!isRoot) {
                appendT(t, subG(s0, t1))
              } else {
                appendT(append(s0, t), t1)
              }

              stepScan(next0(a0), s0, t2)

              // .map { case (s1, a1, t1) =>
              //   val t2 = if(isEmpty(t1)) {
              //     append(s0, t)
              //   } else {
              //     if(!isRoot) {
              //       appendT(t, subG(s0, t1))
              //     } else {
              //       appendT(append(s0, t), t1)
              //     }
              //   }

              //   (s1, a1, t2)
              // }              
              
              /*stepScan(Return(p0), s0, empty).flatMap { case (s1, a1, t1) =>
                val t2 = if(isEmpty(t1)) {
                  append(s0, t)
                } else {
                  if(!isRoot) {
                    appendT(t, subG(s0, t1))
                  } else {
                    appendT(append(s0, t), t1)
                  }
                }
                stepScan(next0(a1), s1, t2)
              }*/
            }

          case ReturnStep(sat) =>
            mo.point(sat).map { case (s, a) => (s, a, t) }

          case ApplyStep(pa, pf, next) =>
            (stepScan(pa, state, empty, idx + 1) |@| stepScan(pf, state, empty, idx + 2)).tupled.map {
              case ((s0, a, t0), (s1, f, t1)) =>
                val s = upd.updateUnmanaged(s0, S.append(s0.unmanaged, s1.unmanaged))
                val tm = merge(t0, t1)
                val tf = appendT(t, tm)
                (s, f(a), tf)
            }.flatMap { case (s0, b, t0) =>
              stepScan(next(b), s0, t0, idx)
            }
        }
      }

      stepScan(this, state, t, idx, true)
    }
*/
    final def run(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[(S, A)] =
      // scan[Unit]((_, _) => (), (_, _) => (), (_, _) => (), (_, _) => (), _ => true, ())(state, ()).map{ case (s, a, t) => (s, a) }
      run0(state)

    final def eval(state: S)(implicit mo: Monad[F], upd: PStateUpdater[Ta, ManagedState, UnmanagedState], S: Semigroup[UnmanagedState]): F[A] =
      run(state).map(_._2)

/*
    final def observe[T](state: S)(
      implicit
        mo: Monad[F],
        upd: PStateUpdater[Ta, ManagedState, UnmanagedState],
        nod: ToNode[S],
        S: Semigroup[UnmanagedState]
    ): F[(S, A, Graph)] = {
      // graph + leaves
      type G = (Graph, Set[String])

      def append(s: S, g: G): G = {
        val node = nod.toNode(s)
        // val id = s.managed.path.last.tags.callee.value + "_" + s.managed.path.last.id.value
        // val node = (id, s.managed.path.last.tags.callee.value)
        val nodes = g._1.nodes + node
        val es = for { c <- g._2 } yield Edge(c, node.id)
        val edges = g._1.edges ++ es
        Graph(nodes, edges) -> Set(node.id)
      }

      def merge(g0: G, g1: G): G = {
        val nodes = g0._1.nodes ++ g1._1.nodes
        val edges = g0._1.edges ++ g1._1.edges
        val g = Graph(nodes, edges)
        g -> g.children.map(_.id)
      }

      def appendT(g0: G, g1: G): G = {
        val nodes = g0._1.nodes ++ g1._1.nodes
        val edges = g0._1.edges ++ g1._1.edges

        val newEdges = for {
          c <- g0._1.children
          p <- g1._1.parents
        } yield Edge(c.id, p.id)

        Graph(nodes, edges ++ newEdges) -> g1._1.children.map(_.id)
      }

      def sub(s: S, g: G): G = {
        val node = nod.toNode(s)
        Graph(Set(Sub("cluster_" + node.id, node.value, g._1)), Set.empty) -> Set(node.id)
      }

      val empty: G = Graph(Set(), Set()) -> Set()

      def isEmpty(g: G) = g match {
        case `empty` => true
        case _ => false
      }

      scan[G](append _, merge _, appendT _, sub _, isEmpty _, empty)(state, empty).map { case (s, a, g) => (s, a, g._1) }
    }
*/
  }


// private [precepte] sealed trait SimplePrecepte[Ta, ManagedState, UnmanagedState, F[_], A] extends Precepte[Ta, ManagedState, UnmanagedState, F, A]
private [precepte] case class Return[Ta, ManagedState, UnmanagedState, F[_], A](a: A) extends Precepte[Ta, ManagedState, UnmanagedState, F, A]
private [precepte] case class Suspend[Ta, ManagedState, UnmanagedState, F[_], A](a: F[A]) extends Precepte[Ta, ManagedState, UnmanagedState, F, A]

private [precepte] case class Map[Ta, ManagedState, UnmanagedState, F[_], I, A](
  sub: Precepte[Ta, ManagedState, UnmanagedState, F, I]
, next: I => A
) extends Precepte[Ta, ManagedState, UnmanagedState, F, A] {
  type _I = I
}

/* A Step followed by a Map (mixes Step + Coyoneda) */
private [precepte] case class StepMap[Ta, ManagedState, UnmanagedState, F[_], I, A](
  st: Precepte[Ta, ManagedState, UnmanagedState, F, A]#S => F[I]
, map: (Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, I) => (Precepte[Ta, ManagedState, UnmanagedState, F, A]#S, A)
, tags: Ta
) extends Precepte[Ta, ManagedState, UnmanagedState, F, A]

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
      def apply[M, U, F[_], A](m: => Precepte[Ta, M, U, F, A])(implicit ap: Applicative[F]): Precepte[Ta, M, U, F, A] =
        apply { (st: Precepte[Ta, M, U, F, A]#S) => ap.point(m) }.flatMap(x => x)

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
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, B]

private [precepte] case class ApplyStep[Ta, ManagedState, UnmanagedState, F[_], A, B, C](
  pa: Precepte[Ta, ManagedState, UnmanagedState, F, A]
, pf: Precepte[Ta, ManagedState, UnmanagedState, F, A => B]
, next: B => Precepte[Ta, ManagedState, UnmanagedState, F, C]
) extends ResumeStep[Ta, ManagedState, UnmanagedState, F, C]
