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
    Version(com.mfglabs.BuildInfo.version))

  val influx = Influx(
    new java.net.URL("http://localhost:8086"),
      "root",
      "root",
      "precepte_sample"
    )

  lazy val logback = Logback(env)

  type Req[A] = (MonitoringContext, Request[A])

  def TimedAction(implicit callee: Callee) =
    new PreActionBuilder[Req, Unit] {
      def initialState = ()
      def version = env.version
      def environment = env.environment
      def host = env.host

      def invokeBlock[A](request: Request[A], block: ((MonitoringContext, Request[A])) => DPre[Future, Unit, Result]) =
        Pre(BaseTags(callee, Category.Api)) { (st: ST[Unit]) => Future.successful(st) } // XXX: does not make sense at the graph level
          .flatMap{ st => block(MonitoringContext(st) -> request) }
          .mapSuspension(influx.monitor)
    }

  case class MonitoringContext(span: Span, path: default.Call.Path[BaseTags]) {
    val logger = logback.Logger(span, path)
  }

  object MonitoringContext {
    def apply[C](st: ST[Unit]): MonitoringContext = MonitoringContext(st.managed.span, st.managed.path)
  }
}


