package com.mfglabs
package precepte

import scala.concurrent.Future


object PreStream {

  type P[T, M, U, A] = Precepte[T, M, U, Future, A]

  def toGraph[T, M, U, A](pre: P[T, M, U, A]) = ???
}