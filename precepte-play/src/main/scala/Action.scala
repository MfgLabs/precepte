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

import play.api._
import play.api.mvc.{ BodyParser, BodyParsers, Result, Request, Action, AnyContent }
import play.api.mvc.Results._

import scala.concurrent.Future
import scala.language.higherKinds

import com.mfglabs.precepte._
import default._

trait PreActionFunction[-R[_], +P[_], F[_], C] {
  self =>
  def invokeBlock[A](request: R[A], block: (ST[C], P[A]) => DPre[F, C, Result]): DPre[F, C, Result]

  protected def executionContext: scala.concurrent.ExecutionContext = play.api.libs.concurrent.Execution.defaultContext

  def andThen[Q[_]](other: PreActionFunction[P, Q, F, C]): PreActionFunction[R, Q, F, C]
}


trait PreActionBuilder[+R[_], C] extends PreActionFunction[Request, R, Future, C] {
  self =>

  def initialState: C
  def version: default.Version
  def environment: default.Environment
  def host: default.Host

  def _env = default.BaseEnv(host, environment, version)
  def initialST = default.ST(Span.gen, _env, Vector.empty, initialState)

  private def addSpan(fr: Future[Result]) = fr.map(_.withHeaders("Span-Id" -> initialST.managed.span.value))(executionContext)

  final def future(block: (ST[C], R[AnyContent]) => Future[Result])(implicit fu: scalaz.Monad[Future], semi: scalaz.Semigroup[C], callee: default.Callee): Action[AnyContent] =
    future(BodyParsers.parse.anyContent)(block)

  final def future[A](bodyParser: BodyParser[A])(block: (ST[C], R[A]) => Future[Result])(implicit fu: scalaz.Monad[Future], semi: scalaz.Semigroup[C], callee: default.Callee): Action[A] =
    Action.async(bodyParser) { r =>
      addSpan(invokeBlock(r, { (mc: ST[C], p: R[A]) =>
        Precepte(default.BaseTags(callee, default.Category.Api)) { st: ST[C] => block(st, p) }
      }).eval(initialST))
    }

  final def async(block: (ST[C], R[AnyContent]) => DPre[Future, C, Result])(implicit fu: scalaz.Monad[Future], semi: scalaz.Semigroup[C], callee: default.Callee): Action[AnyContent] =
    async(BodyParsers.parse.anyContent)(block)

  final def async[A](bodyParser: BodyParser[A])(block: (ST[C], R[A]) => DPre[Future, C, Result])(implicit fu: scalaz.Monad[Future], semi: scalaz.Semigroup[C], callee: default.Callee): Action[A] =
    Action.async(bodyParser) { r =>
      addSpan(Precepte(default.BaseTags(callee, default.Category.Api)) {
        invokeBlock(r, block)
      }.eval(initialST))
    }

  protected def composeParser[A](bodyParser: BodyParser[A]): BodyParser[A] = bodyParser

  // TODO: composition with play action builder ?
  override def andThen[Q[_]](other: PreActionFunction[R, Q, Future, C]): PreActionBuilder[Q, C] = new PreActionBuilder[Q, C] {
    def initialState = self.initialState
    def version = self.version
    def environment = self.environment
    def host = self.host

    def invokeBlock[A](request: Request[A], block: (ST[C], Q[A]) => DPre[Future, C, Result]): DPre[Future, C, Result] =
      self.invokeBlock[A](request, (st, b) => other.invokeBlock[A](b, block))

    override protected def composeParser[A](bodyParser: BodyParser[A]): BodyParser[A] =
      self.composeParser(bodyParser)
  }
}
