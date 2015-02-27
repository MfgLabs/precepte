package monitor

import scala.concurrent.Future
import scalaz.{ Monad, Kleisli, OptionT, MonadTrans, ListT, EitherT, Applicative, Functor }

trait Monitored[A] {
	val f: Context => A
	def apply(c: Context) = f(c)
	def run(c: Context) = apply(c)
}

trait OptionTMonitored[F[_], A] extends Monitored[F[Option[A]]] {
	def T = Monitored.optT[F, A](this)
}

trait ListTMonitored[F[_], A] extends Monitored[F[List[A]]] {
	def T = Monitored.listT[F, A](this)
}

trait LiftableMonitored[F[_], A] extends Monitored[F[A]] {
	def lift[G[_]: Applicative](implicit fu: Functor[F]) =
		Monitored.lift[G].apply(this)
}

trait ToKleisliMonitored[F[_], A] extends Monitored[F[A]] {
	def K = Monitored.toK(this)
}

object Monitored {
	import scalaz.syntax.monad._

	implicit def toOptionT[F[_], A](m: Monitored[F[Option[A]]]) = new OptionTMonitored[F, A] {
		val f = m.f
	}

	implicit def toListT[F[_], A](m: Monitored[F[List[A]]]) = new ListTMonitored[F, A] {
		val f = m.f
	}

	implicit def toLiftableT[F[_], A](m: Monitored[F[A]]) = new LiftableMonitored[F, A] {
		val f = m.f
	}

	implicit def toToKleisliMonitored[F[_], A](m: Monitored[F[A]]) = new ToKleisliMonitored[F, A] {
		val f = m.f
	}

	def toT[F[_], G[_], A, T[_[_], _]](f: F[G[A]] => T[F, A])(m: Monitored[F[G[A]]]): Kleisli[({ type λ[α] = T[F, α] })#λ, Context, A] = {
		type Trans[α] = T[F, α]
		toK(m).mapK[Trans, A](f)
	}

	def toK[F[_], A](m: Monitored[F[A]]) =
		Kleisli(m.f)

	def optT[F[_], A] = toT[F, Option, A, OptionT](OptionT.apply _) _
	def listT[F[_], A] = toT[F, List, A, ListT](ListT.apply _) _

	trait Deferred[G[_]] {
		def apply[F[_]: Functor, A](m: Monitored[F[A]]): Monitored[F[G[A]]]
	}
	def lift[G[_]: Applicative] = new Deferred[G] {
		def apply[F[_]: Functor, A](m: Monitored[F[A]]): Monitored[F[G[A]]] =
			m.map{_.map{ a => implicitly[Applicative[G]].point[A](a) }}
	}

	def apply[A](λ: Context => A): Monitored[A] =
		new Monitored[A] {
			val f = λ
		}

	def id(): Id = ???
	trait Id

	implicit def monitoredInstances[A]: Monad[Monitored] = new Monad[Monitored] {
		def point[A](a: => A) = Monitored(_ => a)
		def bind[A, B](m: Monitored[A])(f: A => Monitored[B]) =
			Monitored(c => f(m.f(c))(c))
	}

}

trait Log {
	def debug(s: String): Unit
}
trait Monitor {
	def time[A](f: => Future[A]): Future[A]
}
case class Context(logger: Log, monitor: Monitor)