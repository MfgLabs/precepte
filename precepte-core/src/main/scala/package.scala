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

import scala.concurrent.{ExecutionContext, Future}

package object precepte {

  /** Transform each Precepte SubStep
    * @tparam T Precepte Tags
    * @tparam M Precepte Managed State
    * @tparam U Precepte Unmanaged State
    * @tparam F Base effect
    */
  type SubStepInstumentation[T, M, U, F[_]] =
    Precepte[T, M, U, F, ?] ~~> Precepte[T, M, U, F, ?]

  /** Dummy implicit to declare Future is trampolined */
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  implicit def trampolinedFuture(implicit ec: ExecutionContext)
    : MetaDefer[Future] with MetaErrorEffect[Throwable, Future] = {
    final class C
        extends MetaErrorEffect[Throwable, Future]
        with MetaDefer[Future] {
      def defer[A](ga: => Future[A]): Future[A] = ga

      def raiseError[A](e: Throwable): Future[A] = Future.failed(e)

      def catchError[A](sub: Future[A])(
          handler: Throwable => Future[A]): Future[A] =
        sub.recoverWith { case e => handler(e) }
    }
    new C
  }
}
