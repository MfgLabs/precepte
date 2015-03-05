package commons

object Monitoring {
	import scala.concurrent.Future

	import play.api.{ Logger => PLog, _ }
	import play.api.mvc._
	import play.api.mvc.Results._

	import com.mfglab.monitoring.Monitored
	import Monitored._

	case class Logger(state: Context.State) {
		private def format(s: String) = s"${state._1.value} -> ${state._2.map(_.value).mkString(":")} $s"
		def debug(message: => String): Unit = PLog.debug(format(message))
	  def info(message: => String): Unit = PLog.info(format(message))
	  def warn(message: => String): Unit = PLog.warn(format(message))
	  def error(message: => String): Unit = PLog.error(format(message))
	}

	case class MonitoringContext(state: Context.State) {
		val logger = Logger(state)
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

				// TODO: Monitor execution timed
				block(request).eval(MonitoringContext.apply _)
			}

		def apply(block: Request[AnyContent] => Monitored[MonitoringContext, Future, Result]): Action[AnyContent] =
			apply(BodyParsers.parse.anyContent)(block)
	}


}