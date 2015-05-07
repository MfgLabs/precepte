package com.mfglabs.monitoring

import play.api.mvc._
import scala.concurrent.Future
import Call._

import scala.concurrent.ExecutionContext
import akka.actor.ActorSystem

case class TimedAction[C](ctx: TaggingContext[BaseEnv, BaseTags, C, Future], influxdbURL: java.net.URL, system: ActorSystem, env: BaseEnv, initialC: C)(implicit ex: ExecutionContext) {
  import ctx._

  val influx = Influx[C](ctx, influxdbURL, env, system)

  private def tagged[A](bodyParser: BodyParser[A])(f: (Tags.Category, Tags.Callee, Request[A]) => Precepte[Result])(implicit fu: scalaz.Monad[Future]) =
    Action.async(bodyParser) { request =>
      import play.api.Routes.{ ROUTE_ACTION_METHOD, ROUTE_CONTROLLER }
      val ts = request.tags

      val name = (for {
        c <- ts.get(ROUTE_CONTROLLER)
        a <- ts.get(ROUTE_ACTION_METHOD)
      } yield s"$c.$a").getOrElse(request.toString)

      val initialState = State[BaseEnv, BaseTags, C](Span.gen, env, Vector.empty, initialC)
      f(Tags.Category.Api, Tags.Callee(name), request).eval(initialState)
    }

  private def cast[A](p: Precepte[A]) = p.asInstanceOf[influx.ctx.Precepte[A]]
  private def uncast[A](p: influx.ctx.Precepte[A]) = p.asInstanceOf[Precepte[A]]

  def apply[A](bodyParser: BodyParser[A])(block: Request[A] => Precepte[Result])(implicit fu: scalaz.Monad[Future]): Action[A] =
    tagged(bodyParser)((c, t, r) => uncast(influx.TimedM(c)(t)(cast(block(r)))))

  def apply(block: Request[AnyContent] => Precepte[Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
    apply(BodyParsers.parse.anyContent)(block)

  def action[A](bodyParser: BodyParser[A])(block: Request[A] => Future[Result])(implicit fu: scalaz.Monad[Future]): Action[A] =
    tagged(bodyParser)((c, t, r) => uncast(influx.Timed(c)(t)(_ => block(r))))

  def action(block: Request[AnyContent] => Future[Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
    action(BodyParsers.parse.anyContent)(block)
}