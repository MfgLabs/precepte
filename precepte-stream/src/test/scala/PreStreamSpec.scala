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

import org.scalatest._
import Matchers._
import org.scalatest.concurrent.{PatienceConfiguration, ScalaFutures}
import akka.actor.ActorSystem
import akka.stream._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scalaz.std.scalaFuture._
import scalaz.syntax.monad._
import default._
import corescalaz._
import org.scalatest.time.Seconds

final class PreStreamSpec extends FlatSpec with ScalaFutures {

  type P[A] = DefaultPre[Future, Unit, A]

  object P {
    def apply[A](tags: BaseTags) = Precepte[BaseTags](tags)
  }

  val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))

  private def tags(n: String) = BaseTags(Callee(n), default.Category.Database)

  def nostate = ST(Span.gen, env, Vector.empty, ())

  val ids = EndlessStream.unfold(BigInt(1L))(n => (PId("CostSpec", n), n + 1))

  implicit val as = ActorSystem()
  implicit val fm = ActorMaterializer()

  implicit val unitSG = new scalaz.Semigroup[Unit] {
    def append(f1: Unit, f2: => Unit) = ()
  }

  "Precepte" should "run/eval simple" in {
    val patience =
      PatienceConfiguration.Timeout(org.scalatest.time.Span(1, Seconds))

    def f1 = P(tags("f1")) { (_: ST[Unit]) =>
      1.point[Future]
    }
    def f2(i: Int) = P(tags("f2")) { (_: ST[Unit]) =>
      s"foo-$i".point[Future]
    }

    def ap1(s: String): P[String] = P(tags("ap1")) { (_: ST[Unit]) =>
      s"$s-1".point[Future]
    }
    def ap2(s: String): P[String] = P(tags("ap2")) { (_: ST[Unit]) =>
      s"$s-2".point[Future]
    }

    val res = for {
      i <- f1
      s <- f2(i)
      r <- (ap1(s) |@| ap2(s)).tupled
    } yield r

    val (_, a) = res.run(nostate).futureValue(patience)
    a should ===("foo-1-1" -> "foo-1-2")

    val flow = PreStream.toFlow(res)
    val p =
      scaladsl.Source.fromIterator(() => Iterator(nostate, nostate)).via(flow)

    p.runForeach {
        case (s, a) =>
          println(s"s:$s a:$a")
          a should ===("foo-1-1" -> "foo-1-2")
          ()
      }
      .futureValue(patience)

  }
}
