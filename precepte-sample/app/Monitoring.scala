package commons

import com.mfglabs.precepte._

object Monitoring {
	import scala.concurrent.Future
	import play.api.Play.current
	import scala.concurrent.ExecutionContext.Implicits.global

	import play.api.{ Logger => PLog, _ }
	import play.api.mvc._
	import play.api.mvc.Results._

	import play.api.libs.json._

  object PreContext extends TaggingContext[BaseEnv, BaseTags, Unit, Future]
  type ST = State[BaseEnv, BaseTags, Unit]

	val env = BaseEnv(
		Tags.Host(java.net.InetAddress.getLocalHost().getHostName()),
		Tags.Environment.Dev,
		Tags.Version(com.mfglabs.BuildInfo.version)
	)
	
	// lazy val influx = Influx(
	// 	PreContext,
	// 	new java.net.URL("http://localhost:8086/db/precepte-sample/series?u=root&p=root"),
	// 	env,
	// 	play.api.libs.concurrent.Akka.system)

	lazy val logback = Logback(env)

	lazy val TimedAction = com.mfglabs.precepte.InfluxTimedAction(())(
		PreContext,
		new java.net.URL("http://localhost:8086/db/precepte-sample/series?u=root&p=root"),
		env,
		play.api.libs.concurrent.Akka.system
	)

	case class MonitoringContext(span: Span, path: com.mfglabs.precepte.Call.Path[BaseTags]) {
		val logger = logback.Logger(span, path)
		val timer = TimedAction.influx.Timer(span, path)
	}

	object MonitoringContext {
		def apply[C](st: State[BaseEnv, BaseTags, C]): MonitoringContext = MonitoringContext(st.span, st.path)
	}
}


