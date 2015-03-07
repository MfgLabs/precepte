package com.mfglab.monitoring

import scala.language.higherKinds

import scala.concurrent.Future
import scalaz.{ Hoist, Monad, Kleisli, OptionT, MonadTrans, ListT, EitherT, Applicative, Functor, ∨ }
import scalaz.Unapply
import scalaz.StateT
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

trait Monitored[C, F[_], A] {
  self =>
  import Monitored.{ Call, Root, Node }

  protected val f: StateT[F ,Call[C], A]

  def eval(fc: Call.State => C, span: Call.Span = Call.Span.gen)(implicit fu: Functor[F]): F[A] =
    f.eval(Root(fc, span, Array.empty))

  def run(fc: Call.State => C, span: Call.Span = Call.Span.gen): F[(Call[C], A)] =
    f.run(Root(fc, span, Array.empty))

  def flatMap[B](fr: A => Monitored[C, F, B])(implicit m: Monad[F]): Monitored[C, F, B] = {
    Monitored[C, F, B](StateT[F, Call[C], B]{ c =>
      val fsa = self.run({ s =>
        c.builder(s)
      })

      val ffb = fr(_: A).run({ (s: Call.State) =>
        val st = Call.State(c.span, s.path)
        c.builder(st)
      })

      for {
        fa <- fsa
        (s0, a) = fa
        fb <- ffb(a)
        (s1, b) = fb
      } yield (c /++ s0.children /++ s1.children, b)
    })
  }

  def map[B](fu: A => B)(implicit m: Functor[F]): Monitored[C, F, B] =
    Monitored[C, F, B] {
      for { r <- f }
      yield fu(r)
    }

  def mapK[G[_], B](fu: F[(Call[C], A)] => G[(Call[C], B)]): Monitored[C, G, B] =
    Monitored[C, G, B] {
      f.mapK(fu)
    }

  def cotrans(implicit ch: CoHasHoist[F], ff: Functor[F]): Monitored[C, ({ type λ[α] = ch.F[α] })#λ, ch.G[A]] =
    ???
    // mapK { a =>
    //   f.map { case (c, ga) =>
    //     ch.unlift(a).map(c -> _)
    //     ???
    //   }
    // }

  def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Monitored[C, F, AP[A]] =
    this.map(a => ap.point(a))
}

object Monitored {
  import scalaz.Id._
  import scalaz.Unapply

  sealed trait Call[C] {
    val children: Array[Node[C]]
    val span: Call.Span
    val builder: Call.State => C
    val path: Array[Call.Segment]
    def /+(n: Node[C]): Call[C] = /++(Array(n))
    def /++(ns: Array[Node[C]]): Call[C]
    def value = builder(Call.State(span, path))
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
    type Segment = (Id, Tags)
    case class State(span: Span, path: Array[Segment])

    object Id {
      def gen = Id(scala.util.Random.alphanumeric.take(7).mkString)
    }
    object Span {
      def gen = Span(java.util.UUID.randomUUID.toString)
    }
  }

  case class Root[C](builder: Call.State => C, span: Call.Span, children: Array[Node[C]]) extends Call[C] {
    val path = Array[Call.Segment]()
    def /++(ns: Array[Node[C]]): Call[C] = {
      val cs = ns.map {
        _.copy(parent = this)
      }
      this.copy(children = children ++ cs)
    }

    override def toString = s"""Root($builder, $span, ${children.toList})"""
  }

  case class Node[C](id: Call.Id, tags: Call.Tags, parent: Call[C], children: Array[Node[C]]) extends Call[C] {
    val span = parent.span
    val builder = parent.builder
    val path = parent.path :+ (id -> tags)
    def /++(ns: Array[Node[C]]): Call[C] = {
      val cs = ns.map {
        _.copy(parent = this)
      }
      this.copy(children = children ++ cs)
    }

    override def toString = s"""Node($id, $tags, <parent>, ${children.toList})"""
  }

  trait *->*[F[_]] {}
  trait *->*->*[F[_, _]] {}

  implicit def fKindEv[F0[_]] = new *->*[F0] {}
  implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}

  trait MonitoredBuilder {
    val tags: Call.Tags

     def apply0[C, A0](λ: Call[C] => A0): Monitored[C, Id, A0] =
      apply[C, Id, A0](λ)

    def apply[C, F0[_]: Functor, A0](λ: Call[C] => F0[A0]): Monitored[C, F0, A0] = {
      new Monitored[C, F0, A0] {
        val f = StateT[F0, Call[C], A0]{ c =>
          val child = Node(Call.Id.gen, tags, c, Array.empty[Node[C]])
          val newC = c /+ child
          λ(child).map { newC -> _ }
        }
      }
    }

    def apply[C, F[_], A](m: Monitored[C, F, A]): Monitored[C, F, A] =
      new Monitored[C, F, A] {
        val f = m.f.contramap{ (c: Call[C]) =>
          ???
        }
      }
  }

  def apply(_tags: Call.Tags) =
    new MonitoredBuilder {
      val tags = _tags
    }

  private def apply[C, F0[_], A0](st: StateT[F0, Call[C], A0]): Monitored[C, F0, A0] =
    new Monitored[C, F0, A0] {
      val f = st
    }

  def trans[C, F[_], G[_]: *->*, A](m: Monitored[C, F, G[A]])(implicit hh: HasHoist[G], fu: Functor[F], fg: Functor[G]): Monitored[C, ({ type λ[α] = hh.T[F, α] })#λ, A] =
    Monitored[C, ({ type λ[α] = hh.T[F, α] })#λ, A](StateT[({ type λ[α] = hh.T[F, α] })#λ, Call[C], A] { c =>
      val fga =
        m.run { st =>
          val st1 = Call.State(c.span, c.path ++ st.path)
          c.builder(st1)
        }.map { case (c, fa) => fa.map(c -> _) }
      hh.lift[F, (Call[C], A)](fga)
    })

  def trans[C, F[_], G[_, _]: *->*->*, A, B](m: Monitored[C, F, G[A, B]])(implicit hh: HasHoist[({ type λ[α] = G[A, α] })#λ], fu: Functor[F], fg: Functor[({ type λ[α] = G[A, α] })#λ]): Monitored[C, ({ type λ[α] = hh.T[F, α] })#λ, B] = {
    type λ[α] = G[A, α]
    trans[C, F, λ, B](m)(new *->*[λ] {}, hh, fu, fg)
  }

  // implicit def monitoredInstances[C, F[_]: Monad] =
  //   new Monad[({ type λ[α] = Monitored[C, F, α] })#λ] {
  //     def point[A](a: => A): Monitored[C, F, A] = Monitored(Context.Tags.empty)[C, F, A]((_: Context[C]) => implicitly[Monad[F]].point(a))
  //     def bind[A, B](m: Monitored[C, F, A])(f: A => Monitored[C, F, B]): Monitored[C, F, B] =
  //       m.flatMap(f)
  //   }
}