package com.mfglabs
package precepte

import org.scalatest._
import Matchers._
import Inspectors._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import scala.language.higherKinds

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scalaz.std.scalaFuture._
import scalaz.syntax.monad._
import scalaz._

import default._


class PreStreamSpec extends FlatSpec with ScalaFutures {

  type P[A] = Pre[Future, Unit, A]

  object P {
    def apply[A](tags: BaseTags) = Precepte[BaseTags](tags)
  }

  val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))

  private def tags(n: String) = BaseTags(Callee(n), default.Category.Database)

  def nostate = ST(Span.gen, env, Vector.empty, ())

  val ids = PIdStream((1 to 30).map(i => PId(i.toString)).toStream)

  implicit val as = ActorSystem()
  implicit val fm = ActorMaterializer()

  implicit val unitSG = new scalaz.Semigroup[Unit] {
    def append(f1: Unit, f2: => Unit) = ()
  }

  "Precepte" should "run/eval simple" in {
    def f1 = P(tags("f1")){(_: ST[Unit]) => 1.point[Future]}
    def f2(i: Int) = P(tags("f2")){(_: ST[Unit]) => s"foo-$i".point[Future]}

    def ap1(s: String): P[String] = P(tags("ap1")){(_: ST[Unit]) => s"$s-1".point[Future]}
    def ap2(s: String): P[String] = P(tags("ap2")){(_: ST[Unit]) => s"$s-2".point[Future]}

    val res = for {
      i <- f1
      s <- f2(i)
      r <- (ap1(s) |@| ap2(s)).tupled
    } yield r

    val (s, a) = res.run(nostate).futureValue
    a should ===("foo-1-1" -> "foo-1-2")

    val flow = PreStream.toFlow(res)
    val p = scaladsl.Source(() => Iterator(nostate, nostate)).via(flow)

    p.runForeach{ case (s, a) =>
      println(s"s:$s a:$a")
      a should ===("foo-1-1" -> "foo-1-2")
    }.futureValue

  }
}