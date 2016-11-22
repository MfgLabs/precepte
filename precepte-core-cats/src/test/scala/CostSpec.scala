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
package corecats

import org.scalatest._
import Matchers._
import Inspectors._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import scala.language.higherKinds

class CostSpec extends FlatSpec with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import cats.std.future._
  import cats.Applicative
  import cats.syntax.flatMap._
  import cats.syntax.apply._
  import cats.data.XorT

  import default._

  type Pre[A] = DefaultPre[Future, Unit, A]

  // object Pre extends DefaultPreBuilder[Future, Unit, Pre]

  val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))

  private def tags(n: String) = BaseTags(Callee(n), Category.Database)

  def nostate = ST(Span.gen, env, Vector.empty, ())

  implicit val unitSG = new cats.Semigroup[Unit] {
    def combine(f1: Unit, f2: Unit) = ()
  }

  val ids = PIdStream((1 to 30).map(i => PId(i.toString)).toStream)


  def timeMs[T](f: => Future[T]): Double = {
    val bef = System.nanoTime
    f.futureValue
    val aft = System.nanoTime

    (aft - bef) / 1000000.0
  }

  "Precepte" should "run/eval simple" in {

    def res(i: Int): Future[Int] = for {
      i <- Applicative[Future].pure(i)
      r <- if(i > 0) res(i-1) else Applicative[Future].pure(i)
    } yield r

    val ms = timeMs(res(10000))
    println(s"Future(10000) -> duration: $ms ms")

    val ms2 = timeMs(res(100000))
    println(s"Future(100000) -> duration: $ms2 ms")

    val ms3 = timeMs(res(1000000))
    println(s"Future(100000) -> duration: $ms3 ms")

  }


  "Precepte" should "run/eval pre" in {

    def res(i: Int): Pre[Int] = for {
      i <- Precepte(tags(s"f$i")){(_: ST[Unit]) => Applicative[Future].pure(i) }
      r <- if(i>0) res(i-1) else Applicative[Pre].pure(i)
    } yield r

    // val ms0 = timeMs(res(1000).eval(nostate))
    // println(s"Pre(1000) -> duration: $ms0 ms")

    val ms = timeMs(res(10000).eval(nostate))
    println(s"Pre(10000) -> duration: $ms ms")

    val ms2 = timeMs(res(100000).eval(nostate))
    println(s"Pre(100000) -> duration: $ms2 ms")

    val ms3 = timeMs(res(1000000).eval(nostate))
    println(s"Pre(1000000) -> duration: $ms3 ms")
  }
}