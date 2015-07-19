package com.mfglabs
package precepte

import org.scalatest._
import Matchers._
import Inspectors._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import scala.language.higherKinds

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scalaz.std.scalaFuture._
import scalaz.syntax.monad._

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._

class PreStreamSpec extends FlatSpec with ScalaFutures {

  import Precepte._

  type P[A] = Precepte0[Future, Unit, A]

  object P {
    def apply[A](tags: BaseTags) = Precepte[BaseTags](tags)
  }

  val env = BaseEnv(Tags.Host("localhost"), Tags.Environment.Test, Tags.Version("1.0"))

  private def tags(n: String) = BaseTags(Tags.Callee(n), Tags.Category.Database)

  def nostate = PST0[Unit](Span.gen, env, Vector.empty, ())

  val ids = PIdStream((1 to 30).map(i => PId(i.toString)).toStream)

  import Tags.Callee

  implicit val as = ActorSystem()
  implicit val fm = ActorMaterializer()

  "Precepte" should "run/eval simple" in {
    def f1 = P(tags("simple.f1")){(_: PST0[Unit]) => 1.point[Future]}
    def f2(i: Int) = P(tags("simple.f2")){(_: PST0[Unit]) => s"foo $i".point[Future]}

    val res = for {
      i <- f1
      r <- f2(i)
    } yield r

    val (s, a) = res.run(nostate).futureValue
    a should ===("foo 1")

    val flow = PreStream.toFlow(res)

    Source(() => Iterator(nostate, nostate)).via(flow).runForeach { case (s, a) =>
      println(s"s:$s a:$a")
      a should ===("foo 1")
    }
  }
}