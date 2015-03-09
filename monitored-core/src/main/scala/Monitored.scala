package com.mfglab.monitoring

import scala.language.higherKinds

import scala.concurrent.Future
import scalaz.{ Hoist, Monad, OptionT, MonadTrans, ListT, EitherT, Applicative, Functor, ∨ }
import com.mfglab.Free
import scala.language.higherKinds
import scalaz.Unapply
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

  private[this] class EitherHasHoist[A] extends HasHoist[({ type λ[α] = A ∨ α })#λ] {
    type T[F[_], B] = EitherT[F, A, B]
    def lift[F[_], B](f: F[A ∨ B]): EitherT[F, A, B] = EitherT.apply(f)
  }

  implicit def eitherHasHoist[A]: HasHoist.Aux[({ type λ[α] = A ∨ α })#λ, ({ type λ[F[_], B] = EitherT[F, A, B] })#λ] = new EitherHasHoist[A]
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
    type G[T] = A ∨ T
    def unlift[B](o: EitherT[F, A, B]): F[A ∨ B] = o.run
  }
}

case class Step[C, F[_], MC](st: IndexedStateT[F, Monitored.Call.State[C], C, MC]) {
  self =>

  import Monitored.Call

  def map[B](fu: MC => B)(implicit ff: Functor[F]): Step[C, F, B] =
    Step[C, F, B](self.st.map(fu))

  def run(state: Call.State[C]): F[(C, MC)] = st.run(state)
}

object Step {
  implicit def stepInstances[C, F[_]: Functor] =
    new Functor[({ type λ[α] = Step[C, F, α] })#λ] {
      override def map[A, B](fa: Step[C, F, A])(f: A => B) = fa map f
    }
}

object Monitored {
  import scalaz.Id._
  import scalaz.Unapply

  type Monitored[C, F[_], A] = Free[({ type λ[α] = Step[C, F, α] })#λ, A]

  case class Call(id: Call.Id) {
    def toJson = ???
  }

  object Call {
    case class Id(value: String) extends AnyVal
    case class Tags(values: (String, String)*) {
      override def toString = s"Tags(${values.toList})"
    }
    object Tags {
      val empty = Tags()
      def Callee(n: String) = ("callee", n)
    }
    type Path = Vector[Call]

    case class Graph[C](id: Call.Id, value: C, children: Vector[Graph[C]])
    case class State[C](path: Path, value: C)

    object Id {
      def gen = Id(scala.util.Random.alphanumeric.take(7).mkString)
    }
  }

  trait *->*[F[_]] {}
  trait *->*->*[F[_, _]] {}

  implicit def fKindEv[F0[_]] = new *->*[F0] {}
  implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}

  import scalaz.{ \/-, -\/ }
  def eval[C, F[_]: Monad, A](m: Monitored.Monitored[C, F, A], state: Call.State[C])(implicit fu: Functor[({ type λ[α] = Step[C, F, α] })#λ]): F[A] =
    m.resume(fu) match {
      case \/-(a) =>
        a.point[F]
      case -\/(step) =>
        step.run(state).flatMap { case (c, mc) =>
          eval(mc, Call.State(state.path :+ Call(Call.Id.gen), c))
        }
    }

  def run[C, F[_]: Monad, A](m: Monitored.Monitored[C, F, A], state: Call.State[C])(implicit fu: Functor[({ type λ[α] = Step[C, F, α] })#λ]): F[(Call.Graph[C], A)] = {
    def go(m: Monitored.Monitored[C, F, A], state: Call.State[C], graph: Call.Graph[C]): F[(Call.Graph[C], A)] = {
      m.resume(fu) match {
        case \/-(a) =>
          (graph, a).point[F]
        case -\/(step) =>
          step.run(state).flatMap {
            case (c, mc: Free.Gosub[({ type λ[α] = Step[C, F, α] })#λ, _]) =>
              val id = Call.Id.gen
              val gn: Call.Graph[C] = graph.copy(children = graph.children :+ Call.Graph(id, c, Vector.empty))
              go(mc, Call.State(state.path :+ Call(id), c), gn)
            case (c, mc) =>
              val id = Call.Id.gen
              go(mc, Call.State(state.path :+ Call(id), c), Call.Graph(id, c, Vector.empty))
          }
      }
    }
    go(m, state, Call.Graph(Call.Id.gen, state.value, Vector.empty))
  }

  def apply0[C, A](λ: Call.State[C] => A): Monitored[C, Id, A] =
    apply[C, Id, A](λ)

  def apply[C, F[_]: Functor, A](λ: Call.State[C] => F[A]): Monitored.Monitored[C, F, A] =
    Free.liftF[({ type λ[α] = Step[C, F, α] })#λ, A] {
      Step[C, F, A] {
        IndexedStateT { (st: Call.State[C]) =>
          for (a <- λ(st))
          yield st.value -> a
        }
      }
    }
}