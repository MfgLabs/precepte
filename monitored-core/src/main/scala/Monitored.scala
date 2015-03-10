package com.mfglab.monitoring

import scala.language.higherKinds

import scala.concurrent.Future
import scalaz.{ Hoist, Monad, OptionT, ListT, EitherT, Applicative, Functor, \/, \/-, -\/ }
import scala.language.higherKinds

import scalaz.IndexedStateT
import scalaz.syntax.monad._

trait HasHoist[M[_]] {
  type T[_[_], _]
  def lift[F[_], A](f: F[M[A]]): T[F, A]
}

object HasHoist {
  type Aux[M[_], T0[_[_], _]] = HasHoist[M] { type T[F[_], A] = T0[F, A] }

  def apply[M[_]](implicit h: HasHoist[M]): Aux[M, h.T] = h

  implicit object optionHasHoist extends HasHoist[Option] {
    type T[F[_], A] = OptionT[F, A]
    def lift[F[_], A](f: F[Option[A]]): OptionT[F, A] = OptionT.apply(f)
  }

  implicit object listHasHoist extends HasHoist[List] {
    type T[F[_], A] = ListT[F, A]
    def lift[F[_], A](f: F[List[A]]): ListT[F, A] = ListT.apply(f)
  }

  private[this] class EitherHasHoist[A] extends HasHoist[({ type λ[α] = A \/ α })#λ] {
    type T[F[_], B] = EitherT[F, A, B]
    def lift[F[_], B](f: F[A \/ B]): EitherT[F, A, B] = EitherT.apply(f)
  }

  implicit def eitherHasHoist[A]: HasHoist.Aux[({ type λ[α] = A \/ α })#λ, ({ type λ[F[_], B] = EitherT[F, A, B] })#λ] = new EitherHasHoist[A]
}

trait CoHasHoist[T[_]] {
  type F[_]
  type G[_]
  def unlift[A](f: T[A]): F[G[A]]
}

object CoHasHoist {
  type Aux[T[_], F0[_], G0[_]] = CoHasHoist[T] {
    type F[X] = F0[X]
    type G[X] = G0[X]
  }

  def apply[T0[_]](implicit ch: CoHasHoist[T0]): Aux[T0, ch.F, ch.G] = ch

  implicit def optionCoHasHoist[F0[_]] = new CoHasHoist[({ type λ[α] = OptionT[F0, α] })#λ] {
    type F[T] = F0[T]
    type G[T] = Option[T]
    def unlift[A](o: OptionT[F, A]): F[Option[A]] = o.run
  }

  implicit def listCoHasHoist[F0[_]] = new CoHasHoist[({ type λ[α] = ListT[F0, α] })#λ] {
    type F[T] = F0[T]
    type G[T] = List[T]
    def unlift[A](o: ListT[F, A]): F[List[A]] = o.run
  }

  implicit def eitherCoHasHoist[F0[_], A] = new CoHasHoist[({ type λ[α] = EitherT[F0, A, α] })#λ] {
    type F[T] = F0[T]
    type G[T] = A \/ T
    def unlift[B](o: EitherT[F, A, B]): F[A \/ B] = o.run
  }
}


sealed trait Monitored[C, F[_], A] {
  self =>
  import Monitored.Call

  final def resume: Monitored[C, F, A] \/ A =
    this match {
      case Return(a) => \/-(a)
      case s@Step(_) => -\/(s)
      case s@Sub(_, _) => -\/(s)
    }

  final def flatMap[B](f: A => Monitored[C, F, B]): Monitored[C, F, B] =
    Sub[C, F, A, B](self, f)

  final def map[B](f: A => B): Monitored[C, F, B] =
    flatMap(a => Return(f(a)))

  final def eval(state: Call.State[C])(implicit mo: Monad[F]): F[A] =
    this match {
      case Return(a) => a.point[F]
      case Step(st) => st.run(state).flatMap { case(c, m) =>
        m.eval(Call.State(state.path :+ Call(Call.Id.gen), c))
      }
      case Sub(sub, next) =>
        sub.eval(state).flatMap { case i =>
          next(i).eval(state)
        }
    }


  final def run(state: Call.State[C])(implicit mo: Monad[F]): F[(Call.Graph[C], A)] = {
    def go[B](m: Monitored[C, F, B], state: Call.State[C], graph: Call.Graph[C]): F[(Call.Graph[C], B)] = {
      m match {
        case Return(a) =>
          (graph, a).point[F]
        case Step(step) =>
          step.run(state).flatMap {
            case (c, mc) =>
              val id = Call.Id.gen
              val g0 = Call.GraphNode(id, c, Vector.empty)
              go(mc, Call.State(state.path :+ Call(id), c), g0).map { case (g, a) =>
                graph.addChild(g) -> a
              }
          }
        case Sub(sub, next) =>
          // XXX: kinda hackish. We're only interested in this node children
          val g0 = Call.GraphNode(Call.Id("dummy"), state.value, Vector.empty)
          go(sub, state, g0).flatMap { case (gi, i) =>
            go(next(i), state, gi).map { case (g, a) =>
              graph.addChildren(g.children) -> a
            }
          }
      }
    }
    val root = Call.Span.gen
    go(this, state, Call.Root(root, Vector.empty))
  }

}

case class Return[C, F[_], A](a: A) extends Monitored[C, F, A]
case class Step[C, F[_], A](st: IndexedStateT[F, Monitored.Call.State[C], C, Monitored[C, F, A]]) extends Monitored[C, F, A] {
  import Monitored.Call
  def run(state: Call.State[C]): F[(C, Monitored[C, F, A])] = st.run(state)
}

case class Sub[C, F[_], I, A](sub: Monitored[C, F, I], next: I => Monitored[C, F, A]) extends Monitored[C, F, A]

object Monitored {
  import scalaz.Id._
  import scalaz.Unapply

  case class Call(id: Call.Id) {
    def toJson = ???
  }

  object Call {
   case class Span(value: String) extends AnyVal
    case class Id(value: String) extends AnyVal
    case class Tags(values: (String, String)*) {
      override def toString = s"Tags(${values.toList})"
    }
    object Tags {
      val empty = Tags()
      def Callee(n: String) = ("callee", n)
    }
    type Path = Vector[Call]

    sealed trait Graph[C] {
      val children: Vector[Graph[C]]
      def addChildren(cs: Vector[Graph[C]]): Graph[C]
      def addChild(c: Graph[C]): Graph[C] =
        addChildren(Vector(c))
    }

    case class GraphNode[C](id: Call.Id, value: C, children: Vector[Graph[C]]) extends Graph[C] {
      def addChildren(cs: Vector[Graph[C]]) =
        this.copy(children = children ++ cs)
    }

    case class Root[C](span: Call.Span, children: Vector[Graph[C]]) extends Graph[C] {
      def addChildren(cs: Vector[Graph[C]]) =
        this.copy(children = children ++ cs)
    }

    case class State[C](path: Path, value: C)

    object Id {
      def gen = Id(scala.util.Random.alphanumeric.take(7).mkString)
    }
    object Span {
      def gen = Span(java.util.UUID.randomUUID.toString)
    }
  }

  def apply0[C, A](λ: Call.State[C] => A): Monitored[C, Id, A] =
    apply[C, Id, A](λ)

  def apply[C, F[_]: Functor, A](λ: Call.State[C] => F[A]): Monitored[C, F, A] =
    Step[C, F, A] {
      IndexedStateT { (st: Call.State[C]) =>
        for (a <- λ(st))
        yield st.value -> Return(a)
      }
    }

  def apply[C, F[_]: Applicative, A](m: Monitored[C, F, A]): Monitored[C, F, A] =
    Step(IndexedStateT[F, Monitored.Call.State[C], C, Monitored[C, F, A]]{ st =>
      (st.value -> m).point[F]
    })

}