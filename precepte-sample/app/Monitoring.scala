package commons

import com.mfglabs.precepte._
import default._

object Monitoring {
	import scala.concurrent.Future
	import play.api.Play.current
	import scala.concurrent.ExecutionContext.Implicits.global

	import play.api.{ Logger => PLog, _ }
	import play.api.mvc._
	import play.api.mvc.Results._

	import play.api.libs.json._


  implicit val unitSG = new scalaz.Semigroup[Unit] {
    def append(f1: Unit, f2: => Unit) = ()
  }

	val env = BaseEnv(
		Host(java.net.InetAddress.getLocalHost().getHostName()),
		Environment.Dev,
		Version(com.mfglabs.BuildInfo.version)
	)
	
	// lazy val influx = Influx(
	// 	PreContext,
	// 	new java.net.URL("http://localhost:8086/db/precepte-sample/series?u=root&p=root"),
	// 	env,
	// 	play.api.libs.concurrent.Akka.system)

	lazy val logback = Logback(env)

	lazy val TimedAction = com.mfglabs.precepte.InfluxTimedAction(())(
		new java.net.URL("http://localhost:8086/db/precepte-sample/series?u=root&p=root"),
		env,
		play.api.libs.concurrent.Akka.system
	)

	case class MonitoringContext(span: Span, path: default.Call.Path[BaseTags]) {
		val logger = logback.Logger(span, path)
		val timer = TimedAction.influx.Timer(span, path)
	}

	object MonitoringContext {
		def apply[C](st: ST[Unit]): MonitoringContext = MonitoringContext(st.managed.span, st.managed.path)
	}
}


