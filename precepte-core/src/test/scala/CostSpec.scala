package com.mfglabs
package precepte

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
  import scalaz.std.scalaFuture._
  import scalaz.syntax.monad._
  import scalaz.EitherT

  import default._

  type P[A] = Pre[Future, Unit, A]

  object P {
    def apply[A](tags: BaseTags) = Precepte[BaseTags](tags)
  }

  val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))

  private def tags(n: String) = BaseTags(Callee(n), Category.Database)

  def nostate = ST(Span.gen, env, Vector.empty, ())

  implicit val unitSG = new scalaz.Semigroup[Unit] {
    def append(f1: Unit, f2: => Unit) = ()
  }

  val ids = PIdStream((1 to 30).map(i => PId(i.toString)).toStream)


  def timeMs[T](f: => Future[T]): Double = {
    val bef = System.nanoTime
    f.futureValue
    val aft = System.nanoTime
    
    (aft - bef) / 1000000.0
  }
/*
  "Precepte" should "run/eval simple" in {

    def res(i: Int): Future[Int] = for {
      i <- i.point[Future]
      r <- if(i > 0) res(i-1) else i.point[Future]
    } yield r

    val ms = timeMs(res(10000))
    println(s"Future(10000) -> duration: $ms ms")

    val ms2 = timeMs(res(100000))
    println(s"Future(100000) -> duration: $ms2 ms")

    val ms3 = timeMs(res(1000000))
    println(s"Future(1000000) -> duration: $ms3 ms")

  }
*/
  "Precepte" should "run/eval pre" in {

    def res(i: Int): P[Int] = for {
      i <- P(tags(s"f$i")){(_: ST[Unit]) => i.point[Future] }
      r <- if(i>0) res(i-1) else i.point[P]
    } yield r

    val ms = timeMs(res(10000).eval(nostate))    
    println(s"Pre(10000) -> duration: $ms ms")

    val ms2 = timeMs(res(100000).eval(nostate))
    println(s"Pre(100000) -> duration: $ms2 ms")

    val ms3 = timeMs(res(1000000).eval(nostate))
    println(s"Pre(1000000) -> duration: $ms3 ms")
  }
}