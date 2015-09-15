/*
Copyright 2015 Mfg labs.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package com.mfglabs
package precepte

import play.api.mvc._
import scala.concurrent.Future

import scala.concurrent.ExecutionContext
import akka.actor.ActorSystem

import default._

object InfluxTimedAction {

  def apply[C : scalaz.Semigroup](initialC: C)(
    influxdbURL: java.net.URL,
    env: BaseEnv,
    system: ActorSystem
  )(implicit ex: ExecutionContext) =
    new InfluxTimedAction(initialC, Influx[C](influxdbURL, env, system))

}

class InfluxTimedAction[C : scalaz.Semigroup](
  val initialC: C, val influx: Influx[C]
)(implicit ex: ExecutionContext) {

  private def tagged[A](bodyParser: BodyParser[A])(f: (Category, Callee, Request[A]) => DPre[Future, C, Result])(implicit fu: scalaz.Monad[Future]) =
    Action.async(bodyParser) { request =>
      import play.api.Routes.{ ROUTE_ACTION_METHOD, ROUTE_CONTROLLER }
      val ts = request.tags

      val name = (for {
        c <- ts.get(ROUTE_CONTROLLER)
        a <- ts.get(ROUTE_ACTION_METHOD)
      } yield s"$c.$a").getOrElse(request.toString)

      val initialState = ST(Span.gen, influx.env, Vector.empty, initialC)
      f(Category.Api, Callee(name), request).eval(initialState)
    }

  // private def cast[A](p: Precepte[A]) = p.asInstanceOf[influx.ctx.Precepte[A]]
  // private def uncast[A](p: influx.ctx.Precepte[A]) = p.asInstanceOf[Precepte[A]]

  def apply[A](bodyParser: BodyParser[A])(block: Request[A] => DPre[Future, C, Result])(implicit fu: scalaz.Monad[Future]): Action[A] =
    tagged(bodyParser)((c, t, r) => influx.TimedM(c)(t)(block(r)))

  def apply(block: Request[AnyContent] => DPre[Future, C, Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
    apply(BodyParsers.parse.anyContent)(block)

  def action[A](bodyParser: BodyParser[A])(block: Request[A] => Future[Result])(implicit fu: scalaz.Monad[Future]): Action[A] =
    tagged(bodyParser)((c, t, r) => influx.Timed(c)(t)(_ => block(r)))

  def action(block: Request[AnyContent] => Future[Result])(implicit fu: scalaz.Monad[Future]): Action[AnyContent] =
    action(BodyParsers.parse.anyContent)(block)
}