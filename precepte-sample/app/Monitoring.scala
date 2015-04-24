package commons

import com.mfglabs.monitoring.{ Precepte, Influx, Logback, Call }
import Precepte._
import Call._

object Monitoring {
	import scala.concurrent.Future
	import play.api.Play.current
	import scala.concurrent.ExecutionContext.Implicits.global

	import play.api.{ Logger => PLog, _ }
	import play.api.mvc._
	import play.api.mvc.Results._

	import play.api.libs.json._

	val env = BaseEnv(Tags.Host(java.net.InetAddress.getLocalHost().getHostName()), Tags.Environment.Dev, Tags.Version(com.mfglabs.BuildInfo.version))
	lazy val influx = Influx(
		new java.net.URL("http://localhost:8086/db/precepte-sample/series?u=root&p=root"),
		env,
		play.api.libs.concurrent.Akka.system)

	lazy val logback = Logback(env)
	val TimedAction = com.mfglabs.monitoring.TimedAction(influx, env)

	case class MonitoringContext(span: Span, path: Path[BaseTags]) {
		val logger = logback.Logger(span, path)
		val timer = influx.Timer(span, path)
	}

	object MonitoringContext {
		def apply[C](st: State[BaseEnv, BaseTags, C]): MonitoringContext = MonitoringContext(st.span, st.path)
	}
}