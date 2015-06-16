package com.mfglabs
package precepte

import play.api.mvc._
import scala.concurrent.Future

import scala.concurrent.ExecutionContext
import akka.actor.ActorSystem

object InfluxTimedAction {
  // def apply[TC <: TaggingContext[BaseEnv, BaseTags, C, Future], C](initialC: C)(
  //   ctx: TC,
  //   influx: Influx[TC, C]
  // )(implicit ex: ExecutionContext) =
  //   new TimedAction(initialC)(ctx, influx)

  def apply[C, TC <: TaggingContext[BaseTags, PStateBase[BaseEnv, BaseTags, C], Future]](initialC: C)(
    ctx: TC,
    influxdbURL: java.net.URL,
    env: BaseEnv,
    system: ActorSystem
  )(implicit ex: ExecutionContext) =
    new InfluxTimedAction(initialC, ctx, Influx[C, TC](ctx, influxdbURL, env, system))
}

class InfluxTimedAction[TC <: TaggingContext[BaseTags, PStateBase[BaseEnv, BaseTags, C], Future], C](
  val initialC: C, val ctx: TC, val influx: Influx[C, TC]
)(implicit ex: ExecutionContext) {
  import ctx._

  private def tagged[A](bodyParser: BodyParser[A])(f: (Tags.Category, Tags.Callee, Request[A]) => TC#Precepte[Result])(implicit fu: scalaz.Monad[Future]) =
    Action.async(bodyParser) { request =>
      import play.api.Routes.{ ROUTE_ACTION_METHOD, ROUTE_CONTROLLER }
      val ts = request.tags

      val name = (for {
        c <- ts.get(ROUTE_CONTROLLER)
        a <- ts.get(ROUTE_ACTION_METHOD)
      } yield s"$c.$a").getOrElse(request.toString)

      val initialState = PStateBase[BaseEnv, BaseTags, C](Span.gen, influx.env, Vector.empty, initialC)
      f(Tags.Category.Api, Tags.Callee(name), request).eval(initialState)
    }

  // private def cast[A](p: Precepte[A]) = p.asInstanceOf[influx.ctx.Precepte[A]]
  // private def uncast[A](p: influx.ctx.Precepte[A]) = p.asInstanceOf[Precepte[A]]

  def apply[A](bodyParser: BodyParser[A])(block: Request[A] => Precepte[Result])(implicit fu: scalaz.Monad[Future]): Action[A] =
    tagged(bodyParser)((c, t, r) => influx.TimedM(c)(t)(block(r)))

  def apply(block: Request[AnyContent] => Precepte[Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
    apply(BodyParsers.parse.anyContent)(block)

  def action[A](bodyParser: BodyParser[A])(block: Request[A] => Future[Result])(implicit fu: scalaz.Monad[Future]): Action[A] =
    tagged(bodyParser)((c, t, r) => influx.Timed(c)(t)(_ => block(r)))

  def action(block: Request[AnyContent] => Future[Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
    action(BodyParsers.parse.anyContent)(block)
}