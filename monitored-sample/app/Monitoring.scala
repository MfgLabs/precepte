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
	val TimedAction = com.mfglabs.monitoring.TimedAction(influx)

	case class MonitoringContext(span: Span, path: Path) {
		val logger = logback.Logger(span, path)
		val timer = influx.Timer(span, path)
	}

	object MonitoringContext {
		def apply[C](st: State[C]): MonitoringContext = MonitoringContext(st.span, st.path)
	}
}