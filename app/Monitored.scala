package monitor

import scala.concurrent.Future
import scalaz.{ Monad, Kleisli, OptionT, MonadTrans, ListT, EitherT }

trait Monitored[A] {
	val f: Context => A
	def apply(c: Context) = f(c)
}

object Monitored {

	private def toT[F[_], G[_], A, T[_[_], _]](f: F[G[A]] => T[F, A])(m: Monitored[F[G[A]]]): Kleisli[({ type λ[α] = T[F, α] })#λ, Context, A] = {
		type Trans[α] = T[F, α]
		Kleisli(m.f).mapK[Trans, A](f)
	}

	def optT[A] = toT[Future, Option, A, OptionT](OptionT.apply _) _
	def listT[A] = toT[Future, List, A, ListT](ListT.apply _) _

	def apply[A](λ: Context => A): Monitored[A] =
		new Monitored[A] {
			val f = λ
		}

	def id(): Id = ???
	trait Id

	implicit def monitoredInstances[A] = new Monad[Monitored] {
		def point[A](a: => A) = Monitored(_ => a)
		def bind[A, B](fa: Monitored[A])(f: A => Monitored[B]) = ???
	}

}

trait Log {
	def debug(s: String): Unit
}
trait Monitor {
	def time[A](f: => Future[A]): Future[A]
}
case class Context(logger: Log, monitor: Monitor)