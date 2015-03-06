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

	case class Logger(state: Context.State) {
		private def format(l: String, s: String) =
			Json.prettyPrint(Json.obj(
				"level" -> l,
				"message" -> s,
				"span" -> state.span.value,
				"path" -> state.path.map { s =>
					Json.obj(
						"id" -> s._1.value,
						"tags" -> s._2.values.map { t =>
								Json.obj(t._1 -> t._2)
							})
				}))

		def debug(message: => String): Unit = PLog.debug(format("debug", message))
	  def info(message: => String): Unit = PLog.info(format("info", message))
	  def warn(message: => String): Unit = PLog.warn(format("warn", message))
	  def error(message: => String): Unit = PLog.error(format("error", message))
	}

	case class Timer(state: Context.State) {
		lazy val path = s"""${state.span} -> / ${state.path.mkString(" / ")}"""
		def timed[A](f: Future[A]) = {
			val t0 = System.nanoTime()
			f.map { x =>
				val t1 = System.nanoTime()
				PLog.debug(s"TIMED: $path: ${(t1 -t0) / 1000000}ms")
				x
			}
		}
	}

	case class MonitoringContext(state: Context.State) {
		val logger = Logger(state)
		val timer = Timer(state)
	}

	def Timed[A](t: Context.Tags)(f: Context[MonitoringContext] => Future[A]) =
		Monitored(t){ (c: Context[MonitoringContext]) =>
			c.value.timer.timed(f(c))
		}

	object TimedAction {
		def apply[A](bodyParser: BodyParser[A])(block: Request[A] => Monitored[MonitoringContext, Future, Result]): Action[A] =
			Action.async(bodyParser) { request =>
				import play.api.Routes.{ ROUTE_ACTION_METHOD, ROUTE_CONTROLLER }
				val ts = request.tags

				val name = (for {
				  c <- ts.get(ROUTE_CONTROLLER)
				  a <- ts.get(ROUTE_ACTION_METHOD)
				} yield s"$c.$a").getOrElse(request.toString)

				Monitored(Context.Tags(Context.Tags.Callee(name))){
					(c: Context[MonitoringContext]) =>
						block(request).mapK(c.value.timer.timed _).eval{ st =>
							c.copy(state = Context.State(c.state.span, c.state.path ++ st.path)).value
						}
				}.eval(MonitoringContext.apply _)
			}

		def apply(block: Request[AnyContent] => Monitored[MonitoringContext, Future, Result]): Action[AnyContent] =
			apply(BodyParsers.parse.anyContent)(block)
	}


}