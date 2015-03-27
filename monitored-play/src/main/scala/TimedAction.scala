package com.mfglabs.monitoring

import play.api.mvc._
import scala.concurrent.Future
import Call._

case class TimedAction(influx: Influx, env: BaseEnv) {
  def apply[A](bodyParser: BodyParser[A])(block: Request[A] => Monitored[BaseEnv, BaseTags, Unit, Future, Result])(implicit fu: scalaz.Monad[Future]): Action[A] =
    Action.async(bodyParser) { request =>
      import play.api.Routes.{ ROUTE_ACTION_METHOD, ROUTE_CONTROLLER }
      val ts = request.tags

      val name = (for {
        c <- ts.get(ROUTE_CONTROLLER)
        a <- ts.get(ROUTE_ACTION_METHOD)
      } yield s"$c.$a").getOrElse(request.toString)

      val initialState = State[BaseEnv, BaseTags, Unit](Span.gen, env, Vector.empty, ())
      influx.TimedM(Tags.Category.Api)(Tags.Callee(name)) {
        block(request)
      }.eval(initialState)
    }

  def apply(block: Request[AnyContent] => Monitored[BaseEnv, BaseTags, Unit, Future, Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
    apply(BodyParsers.parse.anyContent)(block)
}