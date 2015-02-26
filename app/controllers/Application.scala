package controllers

import scala.concurrent.Future
import play.api._
import play.api.mvc._
import monitor._

import scalaz.std.scalaFuture._
import scalaz.syntax.applicative._

import play.api.libs.concurrent.Execution.Implicits._

object models {

	case class User(name: String)
	object User {
		case class Id(value: Long)

		def findById(id: User.Id): Monitored[Future[Option[(User.Id, User)]]] = Monitored { m =>
			import m._
			logger.debug("doing something")
			Some(id -> User("Julien")).point[Future]
		}
	}

	case class Computer(name: String)
	object Computer {
		def forUser(id: User.Id): Monitored[Future[Seq[Computer]]] = Monitored { m =>
			import m._
			logger.debug(s"Getting computers for user $id")
			Seq(Computer("Das Computer"), Computer("Boulier")).point[Future]
		}
	}
}

object MonitoredAction  {
  def apply(f: RequestHeader => Monitored[Future[Result]]): Action[AnyContent] = Action.async { r: RequestHeader =>
  	val logger = new Log {
			def debug(s: String): Unit = Logger.debug(s)
  	}
  	val monitor = new Monitor {
  		def time[A](f: => Future[A]): Future[A] = f
  	}
  	f(r)(Context(logger, monitor))
  }
}

object Application extends Controller {
	import models._
	import scalaz.syntax.monad._

  def index = MonitoredAction { r =>
  	for {
  		u <- User.findById(User.Id(1L))
  		// cs <- Computer.forUser(u._1)
  	} yield Ok(views.html.index("Your new application is ready.")).point[Future]
  }
}