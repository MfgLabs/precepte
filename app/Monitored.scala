package monitor

import scala.concurrent.Future
import scalaz.{ Hoist, Monad, Kleisli, OptionT, MonadTrans, ListT, EitherT, Applicative, Functor, ∨ }

trait HasHoist[M[_]] {
  type T[_[_], _]
  def lift[F[_], A](f: F[M[A]]): T[F, A]
}

object HasHoist {
  def apply[M[_]](implicit h: HasHoist[M]): HasHoist[M] = h

  implicit def optionHasHoist =
  new HasHoist[Option] {
    type T[F[_], A] = OptionT[F, A]
    def lift[F[_], A](f: F[Option[A]]): OptionT[F, A] = OptionT.apply(f)
  }

  // implicit def listHasHoist(implicit h: Hoist[ListT]) = new HasHoist[List, ListT] {
  //   def hoist = h
  //   def lift[F[_], A](f: F[List[A]]): ListT[F, A] = ListT.apply(f)
  // }

  // implicit def eitherHasHoist[A](implicit h: Hoist[({ type λ[α[_], β] = EitherT[α, A, β] })#λ]) =
  //   new HasHoist[({ type λ[α] = A ∨ α })#λ, ({ type λ[α[_], β] = EitherT[α, A, β] })#λ] {
  //     def hoist = h
  //     def lift[F[_], B](f: F[A ∨ B]): EitherT[F, A, B] = EitherT.apply(f)
  //   }
}

trait Monitored[C, FA] {

  type F[_]
  type A

  val f: C => FA
  def apply(c: C) = f(c)
  def run(c: C) = apply(c)

  def T(implicit hh: HasHoist[F]) = ???
}

object Monitored {
  import scalaz.syntax.monad._

  def apply[F0[_], C, A0](λ: C => F0[A0]): Monitored[C, F0[A0]] =
    new Monitored[C, F0[A0]] {
      type F[X] = F0[X]
      type A = A0
      val f = λ
    }

  def lift[C, A0](λ: C => A0): Monitored[C, A0] =
    new Monitored[C, A0] {
      type F[X] = scalaz.Id.Id[X]
      type A = A0
      val f = λ
    }


  // implicit def monitoredInstances[C, A]: Monad[({ type λ[α] = Monitored[C, α] })#λ] =
  //   new Monad[({ type λ[α] = Monitored[C, α] })#λ] {
  //     def point[A](a: => A) = Monitored(_ => a)
  //     def bind[A, B](m: Monitored[C, A])(f: A => Monitored[C, B]) =
  //       Monitored(c => f(m(c))(c))
  //   }

}