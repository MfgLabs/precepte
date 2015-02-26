package monitor

import scala.concurrent.Future
import scalaz.{ Monad, Kleisli, OptionT, MonadTrans, ListT, EitherT }

trait Monitored[A] {
	val f: Context => A
	def apply(c: Context) = f(c)
}

trait OptionTMonitored[F[_], A] extends Monitored[F[Option[A]]] {
	def T = Monitored.optT[F, A](this)
}

trait ListTMonitored[F[_], A] extends Monitored[F[List[A]]] {
	def T = Monitored.listT[F, A](this)
}

object Monitored {
	implicit def toOptionT[F[_], A](m: Monitored[F[Option[A]]]) = new OptionTMonitored[F, A] {
		val f = m.f
	}

	implicit def toListT[F[_], A](m: Monitored[F[List[A]]]) = new ListTMonitored[F, A] {
		val f = m.f
	}

	private def toT[F[_], G[_], A, T[_[_], _]](f: F[G[A]] => T[F, A])(m: Monitored[F[G[A]]]): Kleisli[({ type λ[α] = T[F, α] })#λ, Context, A] = {
		type Trans[α] = T[F, α]
		Kleisli(m.f).mapK[Trans, A](f)
	}

	def optT[F[_], A] = toT[F, Option, A, OptionT](OptionT.apply _) _
	def listT[F[_], A] = toT[F, List, A, ListT](ListT.apply _) _

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