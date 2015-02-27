package monitor

import scala.concurrent.Future
import scalaz.{ Monad, Kleisli, OptionT, MonadTrans, ListT, EitherT, Applicative, Functor, ∨ }

trait Monitored[C, A] {
  val f: C => A
  def apply(c: C) = f(c)
  def run(c: C) = apply(c)
}

trait OptionTMonitored[C, F[_], A] extends Monitored[C, F[Option[A]]] {
  def T = Monitored.optT[C, F, A](this)
}

trait ListTMonitored[C, F[_], A] extends Monitored[C, F[List[A]]] {
  def T = Monitored.listT[C, F, A](this)
}

trait EitherTMonitored[C, F[_], A, B] extends Monitored[C, F[A ∨ B]] {
  def T = Monitored.eitherT[C, F, A, B](this)
}

trait LiftableMonitored[C, F[_], A] extends Monitored[C, F[A]] {
  def lift[G[_]: Applicative](implicit fu: Functor[F]) =
    Monitored.lift[G].apply(this)
}

trait ToKleisliMonitored[C, F[_], A] extends Monitored[C, F[A]] {
  def K = Monitored.toK(this)
}

object Monitored {
  import scalaz.syntax.monad._

  implicit def toOptionT[C, F[_], A](m: Monitored[C, F[Option[A]]]) = new OptionTMonitored[C, F, A] {
    val f = m.f
  }

  implicit def toListT[C, F[_], A](m: Monitored[C, F[List[A]]]) = new ListTMonitored[C, F, A] {
    val f = m.f
  }

  implicit def toEitherT[C, F[_], A, B](m: Monitored[C, F[A ∨ B]]) = new EitherTMonitored[C, F, A, B] {
    val f = m.f
  }

  implicit def toLiftableT[C, F[_], A](m: Monitored[C, F[A]]) = new LiftableMonitored[C, F, A] {
    val f = m.f
  }

  implicit def toToKleisliMonitored[C, F[_], A](m: Monitored[C, F[A]]) = new ToKleisliMonitored[C, F, A] {
    val f = m.f
  }

  def toT[C, F[_], G[_], A, T[_[_], _]](f: F[G[A]] => T[F, A])(m: Monitored[C, F[G[A]]]): Kleisli[({ type λ[α] = T[F, α] })#λ, C, A] = {
    type Trans[α] = T[F, α]
    toK(m).mapK[Trans, A](f)
  }

  def toK[C, F[_], A](m: Monitored[C, F[A]]) =
    Kleisli(m.f)

  def optT[C, F[_], A] = toT[C, F, Option, A, OptionT](OptionT.apply _) _
  def listT[C, F[_], A] = toT[C, F, List, A, ListT](ListT.apply _) _

  def eitherT[C, F[_], A, B](m: Monitored[C, F[A ∨ B]]): Kleisli[({ type λ[α] = EitherT[F, A, α] })#λ, C, B] = {
    type Trans[α] = EitherT[F, A, α]
    toK(m).mapK[Trans, B](EitherT.apply _)
  }


  trait Deferred[G[_]] {
    def apply[C, F[_]: Functor, A](m: Monitored[C, F[A]]): Monitored[C, F[G[A]]]
  }
  def lift[G[_]: Applicative] = new Deferred[G] {
    def apply[C, F[_]: Functor, A](m: Monitored[C, F[A]]): Monitored[C, F[G[A]]] =
      m.map{_.map{ a => implicitly[Applicative[G]].point[A](a) }}
  }

  def apply[C, A](λ: C => A): Monitored[C, A] =
    new Monitored[C, A] {
      val f = λ
    }

  def id(): Id = ???
  trait Id

  implicit def monitoredInstances[C, A]: Monad[({ type λ[α] = Monitored[C, α] })#λ] = new Monad[({ type λ[α] = Monitored[C, α] })#λ] {
    def point[A](a: => A) = Monitored(_ => a)
    def bind[A, B](m: Monitored[C, A])(f: A => Monitored[C, B]) =
      Monitored(c => f(m.f(c))(c))
  }

}