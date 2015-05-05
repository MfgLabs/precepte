package com.mfglabs.monitoring

import play.api.mvc._
import scala.concurrent.Future
import Call._

case class TimedAction(influx: Influx, env: BaseEnv) {
  private def tagged[A](bodyParser: BodyParser[A])(f: (Tags.Category, Tags.Callee, Request[A]) => Precepte.Aux[BaseEnv, BaseTags, Unit, Future, Result])(implicit fu: scalaz.Monad[Future]) =
    Action.async(bodyParser) { request =>
      import play.api.Routes.{ ROUTE_ACTION_METHOD, ROUTE_CONTROLLER }
      val ts = request.tags

      val name = (for {
        c <- ts.get(ROUTE_CONTROLLER)
        a <- ts.get(ROUTE_ACTION_METHOD)
      } yield s"$c.$a").getOrElse(request.toString)

      val initialState = State[BaseEnv, BaseTags, Unit](Span.gen, env, Vector.empty, ())
      f(Tags.Category.Api, Tags.Callee(name), request).eval(initialState)
    }

  def apply[A](bodyParser: BodyParser[A])(block: Request[A] => Precepte.Aux[BaseEnv, BaseTags, Unit, Future, Result])(implicit fu: scalaz.Monad[Future]): Action[A] =
    tagged(bodyParser)((c, t, r) => influx.TimedM(c)(t)(block(r)))

  def apply(block: Request[AnyContent] => Precepte.Aux[BaseEnv, BaseTags, Unit, Future, Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
    apply(BodyParsers.parse.anyContent)(block)

  def action[A](bodyParser: BodyParser[A])(block: Request[A] => Future[Result])(implicit fu: scalaz.Monad[Future]): Action[A] =
    tagged(bodyParser)((c, t, r) => influx.Timed(c)(t)(_ => block(r)))

  def action(block: Request[AnyContent] => Future[Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
    action(BodyParsers.parse.anyContent)(block)
}