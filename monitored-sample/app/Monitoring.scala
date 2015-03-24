package commons

import com.mfglabs.monitoring.{ Monitored, Influx }
import Monitored._
import Monitored.Call._

object Monitoring {
	import scala.concurrent.Future
	import play.api.Play.current
	import scala.concurrent.ExecutionContext.Implicits.global

	import play.api.{ Logger => PLog, _ }
	import play.api.mvc._
	import play.api.mvc.Results._

	import play.api.libs.json._

	lazy val influx = Influx(
		new java.net.URL("http://localhost:8086/db/monitored-sample/series?u=root&p=root"),
		Tags.Environment.Dev,
		Tags.Host(java.net.InetAddress.getLocalHost().getHostName()),
		play.api.libs.concurrent.Akka.system)

	case class Logger(span: Span, path: Path) {
		private def format(l: String, s: String) =
			Json.prettyPrint(Json.obj(
				"level" -> l,
				"message" -> s,
				"span" -> span.value,
				"path" -> path.map { s =>
					Json.obj(
						"id" -> s.id.value,
						"tags" -> s.tags.values.map { t =>
								Json.obj(t.name -> t.value)
							})
				}))

		def debug(message: => String): Unit = PLog.debug(format("debug", message))
	  def info(message: => String): Unit = PLog.info(format("info", message))
	  def warn(message: => String): Unit = PLog.warn(format("warn", message))
	  def error(message: => String): Unit = PLog.error(format("error", message))
	}

	case class MonitoringContext(span: Span, path: Path) {
		val logger = Logger(span, path)
		val timer = influx.Timer(span, path)
	}

	object MonitoringContext {
		def apply[C](st: State[C]): MonitoringContext = MonitoringContext(st.span, st.path)
	}

	def Timed[A](category: Tags.Category)(callee: Tags.Callee, others: Tags = Tags.empty)(f: State[Unit] => Future[A])(implicit fu: scalaz.Functor[Future]): Monitored[Unit, Future, A] =
		Monitored(Tags(category, callee) ++ others){ (c: State[Unit]) =>
			influx.Timer(c.span, c.path).timed(f(c))
		}

	def TimedM[A](category: Tags.Category)(callee: Tags.Callee, others: Tags = Tags.empty)(f: Monitored[Unit, Future, A])(implicit mo: scalaz.Monad[Future]): Monitored[Unit, Future, A] =
		Monitored(Tags(category, callee) ++ others){ (c: State[Unit]) =>
			influx.Timer(c.span, c.path).timed(f.eval(c))
		}

	object TimedAction {
		def apply[A](bodyParser: BodyParser[A])(block: Request[A] => Monitored[Unit, Future, Result])(implicit fu: scalaz.Monad[Future]): Action[A] =
			Action.async(bodyParser) { request =>
				import play.api.Routes.{ ROUTE_ACTION_METHOD, ROUTE_CONTROLLER }
				val ts = request.tags

				val name = (for {
				  c <- ts.get(ROUTE_CONTROLLER)
				  a <- ts.get(ROUTE_ACTION_METHOD)
				} yield s"$c.$a").getOrElse(request.toString)

				val initialState = State(Span.gen, Vector.empty, ())
				TimedM(Tags.Category.Api)(Tags.Callee(name)) {
					block(request)
				}.eval(initialState)
			}

		def apply(block: Request[AnyContent] => Monitored[Unit, Future, Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
			apply(BodyParsers.parse.anyContent)(block)
	}


}