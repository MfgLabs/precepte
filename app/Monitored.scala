package monitor

import scala.concurrent.Future
import scalaz.{ Hoist, Monad, Kleisli, OptionT, MonadTrans, ListT, EitherT, Applicative, Functor, ∨ }
import scalaz.Leibniz.{===, refl}
import scalaz.Id._
import scalaz.Unapply

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

trait Monitored[C, F[_], A] {

  val f: C => F[A]
  def apply(c: C): F[A] = f(c)

  def flatMap[B](fr: A => Monitored[C, F, B])(implicit m: Monad[F]) =
    Monitored[C, F, B] { (c: C) =>
      m.bind(apply(c))(a => fr(a)(c))
    }

  def map[B](fu: A => B)(implicit m: Functor[F]) =
    Monitored[C, F, B] { (c: C) =>
      m.map(f(c))(fu)
    }

  def lift[AP[_]](implicit ap: Applicative[AP], fu: Functor[F]): Monitored[C, F, AP[A]] =
    this.map(a => ap.point(a))
}

object Monitored {
  import scalaz.syntax.monad._
  import scalaz.Id._
  import scalaz.Unapply

  trait *->*[F[_]] {}
  trait *->*->*[F[_, _]] {}

  implicit def fKindEv[F0[_]] = new *->*[F0] {}
  implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}

  def apply0[C, A0](λ: C => A0): Monitored[C, Id, A0] =
    apply[C, Id, A0](λ)

  def apply[C, F0[_], A0](λ: C => F0[A0]): Monitored[C, F0, A0] =
    new Monitored[C, F0, A0] {
      val f = λ
    }

  def trans[C, F[_], G[_]: *->*, A](m: Monitored[C, F, G[A]])(implicit hh: HasHoist[G]): Monitored[C, ({ type λ[α] = hh.T[F, α] })#λ, A] =
    Monitored[C, ({ type λ[α] = hh.T[F, α] })#λ, A] { (c: C) =>
      hh.lift[F, A](m.f(c))
    }

  def trans[C, F[_], G[_, _]: *->*->*, A, B](m: Monitored[C, F, G[A, B]])(implicit hh: HasHoist[({ type λ[α] = G[A, α] })#λ]): Monitored[C, ({ type λ[α] = hh.T[F, α] })#λ, B] = {
    type λ[α] = G[A, α]
    trans[C, F, λ, B](m)(new *->*[λ] {}, hh)
  }

  // implicit def monitoredInstances[C, F[_]: Monad, A] =
  //   new Monad[({ type λ[α] = Monitored[C, F[α]] })#λ] {
  //     def point[A](a: => A): Monitored[C, F[A]] = Monitored.apply1[C, F, A](_ => implicitly[Monad[F]].point(a))
  //     def bind[A, B](m: Monitored[C, F[A]])(f: A => Monitored[C, F[B]]): Monitored[C, F[B]] =
  //       m.flatMap(f)
  //   }
}