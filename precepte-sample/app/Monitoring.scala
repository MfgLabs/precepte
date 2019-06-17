package commons

import com.mfglabs.precepte._
import corescalaz._
import default._

object Monitoring {
  import scala.concurrent.Future
  import play.api.mvc._

  implicit val unitSG = new scalaz.Semigroup[Unit] {
    def append(f1: Unit, f2: => Unit) = ()
  }

  val env = BaseEnv(
    Host(java.net.InetAddress.getLocalHost().getHostName()),
    com.mfglabs.precepte.default.Environment.Dev,
    Version(com.mfglabs.BuildInfo.version)
  )

  val influx = new Influx(
    new java.net.URL("http://localhost:8086"),
    "root",
    "root",
    "precepte_sample",
    "autogen"
  )

  lazy val logback = new Logback(env, "application")

  type Req[A] = (MonitoringContext, Request[A])

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def TimedAction(implicit callee: Callee) =
    new PreActionFunction[Request, Req, Future, Unit] {
      def invokeBlock[A](request: Request[A],
                         block: (
                             (MonitoringContext, Request[A])) => DPre[Future,
                                                                      Unit,
                                                                      Result]) =
        Precepte(BaseTags(callee, Category.Api)) { (st: ST[Unit]) =>
          Future.successful(st)
        } // XXX: does not make sense at the graph level
          .flatMap { st =>
            block(MonitoringContext(st) -> request)
          }
      // TODO: enable InfluxDB on Travis CI
      //.mapSuspension(influx.monitor)
    }

  final case class MonitoringContext(span: Span,
                                     path: default.Call.Path[BaseTags]) {
    val logger = new logback.Logger(span, path)
  }

  object MonitoringContext {
    def apply[C](st: ST[Unit]): MonitoringContext =
      MonitoringContext(st.managed.span, st.managed.path)
  }
}
