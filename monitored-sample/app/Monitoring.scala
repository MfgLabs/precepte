package commons

import com.mfglabs.monitoring.{ Monitored, Influx, Logback }
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

	val env = Tags.Environment.Dev
	val host = Tags.Host(java.net.InetAddress.getLocalHost().getHostName())
	lazy val influx = Influx(
		new java.net.URL("http://localhost:8086/db/monitored-sample/series?u=root&p=root"),
		env,
		host,
		play.api.libs.concurrent.Akka.system)

	lazy val logback = Logback(env)

	case class MonitoringContext(span: Span, path: Path) {
		val logger = logback.Logger(span, path)
		val timer = influx.Timer(span, path)
	}

	object MonitoringContext {
		def apply[C](st: State[C]): MonitoringContext = MonitoringContext(st.span, st.path)
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
				influx.TimedM(Tags.Category.Api)(Tags.Callee(name)) {
					block(request)
				}.eval(initialState)
			}

		def apply(block: Request[AnyContent] => Monitored[Unit, Future, Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
			apply(BodyParsers.parse.anyContent)(block)
	}


}