package commons

object Monitoring {
	import scala.concurrent.Future
	import scala.concurrent.ExecutionContext.Implicits.global

	import play.api.{ Logger => PLog, _ }
	import play.api.mvc._
	import play.api.mvc.Results._

	import play.api.libs.json._

	import com.mfglab.monitoring.Monitored
	import Monitored._
	import Monitored.Call._

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
								Json.obj(t._1 -> t._2)
							})
				}))

		def debug(message: => String): Unit = PLog.debug(format("debug", message))
	  def info(message: => String): Unit = PLog.info(format("info", message))
	  def warn(message: => String): Unit = PLog.warn(format("warn", message))
	  def error(message: => String): Unit = PLog.error(format("error", message))
	}

	case class Timer(span: Span, path: Path) {
		lazy val p = s"""${span} -> / ${path.map { c => c.tags.values.mkString("[", ", ", "]") + "(" + c.id.value + ")" }.mkString(" / ") }"""
		def timed[A](f: Future[A]) = {
			val t0 = System.nanoTime()
			f.map { x =>
				val t1 = System.nanoTime()
				// PLog.debug(s"TIMED: $p: ${(t1 -t0) / 1000000}ms")
				x
			}
		}
	}

	case class MonitoringContext(span: Span, path: Path) {
		val logger = Logger(span, path)
		val timer = Timer(span, path)
	}

	object MonitoringContext {
		def apply[C](st: State[C]): MonitoringContext = MonitoringContext(st.span, st.path)
	}

	def Timed[A](t: Tags)(f: State[Unit] => Future[A])(implicit fu: scalaz.Functor[Future]): Monitored[Unit, Future, A] =
		Monitored(t){ (c: State[Unit]) =>
			Timer(c.span, c.path).timed(f(c))
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

				val tags = Tags(Tags.Callee(name))
				val initialState = State(Span.gen, Vector.empty, ())

				Monitored(tags) {
					block(request)
				}.eval(initialState)
			}

		def apply(block: Request[AnyContent] => Monitored[Unit, Future, Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
			apply(BodyParsers.parse.anyContent)(block)
	}


}