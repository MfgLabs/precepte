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

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import default._

trait PreActionFunction[-R[_], +P[_], F[_], C] {
  self =>
  def invokeBlock[A](request: R[A],
                     block: P[A] => DPre[F, C, Result]): DPre[F, C, Result]

  @inline final def andThen[Q[_]](
      other: PreActionFunction[P, Q, F, C]): PreActionFunction[R, Q, F, C] =
    new PreActionFunction[R, Q, F, C] {
      def invokeBlock[A](
          request: R[A],
          block: Q[A] => DPre[F, C, Result]): DPre[F, C, Result] =
        self.invokeBlock[A](request, b => other.invokeBlock[A](b, block))
    }
}

trait PreActionSyntax[C] {
  self =>

  final type PrePlayAction[R[_]] = PreActionFunction[Request, R, Future, C]

  def initialState: C
  def version: default.Version
  def environment: default.Environment
  def host: default.Host

  @inline final def _env: BaseEnv = default.BaseEnv(host, environment, version)
  @inline final def initialST
    : PState[BaseTags, ManagedState[BaseEnv, BaseTags], C] =
    default.ST(Span.gen, _env, Vector.empty, initialState)

  protected def executionContext: scala.concurrent.ExecutionContext
  protected def controllerComponent: play.api.mvc.ControllerComponents

  def _transform: (Future[Result] => Future[Result]) = identity

  @inline final def transform(
      f: Future[Result] => Future[Result]): PreActionSyntax[C] =
    new PreActionSyntax[C] {
      def initialState = self.initialState
      def version = self.version
      def environment = self.environment
      def host = self.host

      def executionContext = self.executionContext
      def controllerComponent = self.controllerComponent

      override def _transform = self._transform andThen f
    }

  private def addSpan[T](st: default.ST[T])(fr: Future[Result]) =
    fr.map(_.withHeaders("Span-Id" -> st.managed.span.value))(executionContext)

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  final def future[R[_]](fun: PrePlayAction[R])(
      block: R[AnyContent] => Future[Result])(
      implicit mo: MetaMonad[Future],
      semi: MetaSemigroup[C],
      callee: default.Callee): Action[AnyContent] =
    future(controllerComponent.parsers.anyContent)(fun)(block)

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  final def future[R[_], A](bodyParser: BodyParser[A])(fun: PrePlayAction[R])(
      block: R[A] => Future[Result])(implicit mo: MetaMonad[Future],
                                     semi: MetaSemigroup[C],
                                     callee: default.Callee): Action[A] =
    controllerComponent.actionBuilder.async(bodyParser) { r =>
      val st = initialST
      val f = addSpan(st)(
        fun
          .invokeBlock(r, { p: R[A] =>
            Precepte(default.BaseTags(callee, default.Category.Api)) {
              _: ST[C] =>
                block(p)
            }
          })
          .eval(st))
      _transform(f)
    }

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  final def async[R[_]](fun: PrePlayAction[R])(
      block: R[AnyContent] => DPre[Future, C, Result])(
      implicit mo: MetaMonad[Future],
      semi: MetaSemigroup[C]): Action[AnyContent] =
    async(controllerComponent.parsers.anyContent)(fun)(block)

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  final def async[R[_], A](bodyParser: BodyParser[A])(fun: PrePlayAction[R])(
      block: R[A] => DPre[Future, C, Result])(
      implicit mo: MetaMonad[Future],
      semi: MetaSemigroup[C]): Action[A] =
    controllerComponent.actionBuilder.async(bodyParser) { r =>
      val st = initialST
      val f = addSpan(st)(fun.invokeBlock(r, block).eval(st))
      _transform(f)
    }
}

object PreActionSyntax {
  def apply[C](st: C,
               v: default.Version,
               e: default.Environment,
               h: default.Host,
               cc: ControllerComponents): PreActionSyntax[C] =
    new PreActionSyntax[C] {
      def initialState: C = st
      def version: Version = v
      def environment: Environment = e
      def host: Host = h

      protected def executionContext: ExecutionContext = cc.executionContext
      protected def controllerComponent: ControllerComponents = cc
    }
}
