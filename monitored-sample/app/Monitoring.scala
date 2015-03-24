package commons

import com.mfglabs.monitoring.Monitored
import Monitored._
import Monitored.Call._

object Influx {
	import akka.actor.{ Actor, Props }
	import play.api.libs.concurrent.Akka.system
	import scala.concurrent.duration._
	import scala.concurrent.ExecutionContext
	import play.api.libs.ws._
	import java.net.URL

	import play.api.Play.current // xxx
	import scala.concurrent.ExecutionContext.Implicits.global // xxx

	case object Publish

	private class InfluxClient(influxdbURL: URL) extends Actor {
		type Metric = (Long, Span, Path, Duration)
		val metrics = scala.collection.mutable.ArrayBuffer[Metric]()

		private def json: String = {
			val sep = "/"

			val points =
				metrics.map { case (time, span, path, duration) =>
					val p = path.map { c =>
						c.id.value
					}.mkString(sep, sep, "")

					val callees =
						path.flatMap { c =>
							c.tags.values.collect { case Tags.Callee(n) => n }
						}.mkString(sep, sep, "")

					val category =
						path.last.tags.values.collect { case Tags.Category(c) => c }.head

					s"""["${Monitoring.hostname}", "${Monitoring.env.value}", "$category", "${span.value}", "$p", "$callees", $time, ${duration.toNanos}]"""
				}.mkString(",")

			s"""[{"name": "response_times", "columns": ["host", "environment", "category", "span", "path", "callees", "time", "duration"], "points": [$points] }]"""
		}

		def receive = {
			case m: Metric =>
				metrics += m
			case Publish =>
				if(metrics.nonEmpty) {
					WS.url(influxdbURL.toString).post(json)
					metrics.clear()
				}
		}
	}

	private val client = system.actorOf(Props[InfluxClient](
		new InfluxClient(new URL("http://localhost:8086/db/monitored-sample/series?u=root&p=root"))))
	system.scheduler.schedule(10 seconds, 10 seconds, client, Publish)

	case class Timer(span: Span, path: Path) {
		def timed[A](f: scala.concurrent.Future[A])(implicit ex: ExecutionContext) = {
			val t0 = System.nanoTime()
			f.map { x =>
				val t1 = System.nanoTime()
				client ! (t0 / 1000000, span, path, (t1 -t0) nanoseconds)
				x
			}
		}
	}
}

object Monitoring {
	import scala.concurrent.Future
	import scala.concurrent.ExecutionContext.Implicits.global

	import play.api.{ Logger => PLog, _ }
	import play.api.mvc._
	import play.api.mvc.Results._

	import play.api.libs.json._

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
								Json.obj(t.name -> t.value)
							})
				}))

		def debug(message: => String): Unit = PLog.debug(format("debug", message))
	  def info(message: => String): Unit = PLog.info(format("info", message))
	  def warn(message: => String): Unit = PLog.warn(format("warn", message))
	  def error(message: => String): Unit = PLog.error(format("error", message))
	}

	case class MonitoringContext(span: Span, path: Path) {
		val logger = Logger(span, path)
		val timer = Influx.Timer(span, path)
	}

	object MonitoringContext {
		def apply[C](st: State[C]): MonitoringContext = MonitoringContext(st.span, st.path)
	}

	def Timed[A](category: Tags.Category)(callee: Tags.Callee, others: Tags = Tags.empty)(f: State[Unit] => Future[A])(implicit fu: scalaz.Functor[Future]): Monitored[Unit, Future, A] =
		Monitored(Tags(category, callee) ++ others){ (c: State[Unit]) =>
			Influx.Timer(c.span, c.path).timed(f(c))
		}

	def TimedM[A](category: Tags.Category)(callee: Tags.Callee, others: Tags = Tags.empty)(f: Monitored[Unit, Future, A])(implicit mo: scalaz.Monad[Future]): Monitored[Unit, Future, A] =
		Monitored(Tags(category, callee) ++ others){ (c: State[Unit]) =>
			Influx.Timer(c.span, c.path).timed(f.eval(c))
		}

	val hostname = java.net.InetAddress.getLocalHost().getHostName()
	val env = Tags.Environment.Dev

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
				TimedM(Tags.Category.Api)(Tags.Callee(name)) {
					block(request)
				}.eval(initialState)
			}

		def apply(block: Request[AnyContent] => Monitored[Unit, Future, Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
			apply(BodyParsers.parse.anyContent)(block)
	}


}