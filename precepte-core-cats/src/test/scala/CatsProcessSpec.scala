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


class CatsProcessSpec extends FlatSpec with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import cats.Applicative
  import cats.std.future._
  import cats.syntax.flatMap._
  import cats.syntax.apply._
  import cats.data.{XorT, StreamingT}

  import default._

  type Pre[A] = DefaultPre[Future, Unit, A]

  val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))

  private def tags(n: String) = BaseTags(Callee(n), Category.Database)

  it should "observe" in {
    def nostate = ST(Span.gen, env, Vector.empty, 0)

    implicit val intSG = new cats.Semigroup[Int] {
      def combine(f1: Int, f2: Int) = f1 + f2
    }

    // val canbe = Precepte(tags("can be")).apply((s: ST[Int]) => Future(1))
    // val persian = Precepte(tags("persian")).apply((s: ST[Int]) => Future(0))
    // val meets = Precepte(tags("meets")).apply((s: ST[Int]) => Future(1))
    // val sphynx = Precepte(tags("sphynx")).apply((s: ST[Int]) => Future(2))
    // val gives = Precepte(tags("gives")).apply((s: ST[Int]) => Future(2))
    // val mainCoon = Precepte(tags("main coon")).apply((s: ST[Int]) => Future(3))
    // val burmese = Precepte(tags("burmese")).apply((s: ST[Int]) => Future(4))
    // val munchkin = Precepte(tags("munchkin")).apply((s: ST[Int]) => Future(5))
    // val tabby = Precepte(tags("tabby")).apply((s: ST[Int]) => Future(6))
    // val himalayan = Precepte(tags("himalayan")).apply((s: ST[Int]) => Future(7))
    // val but = Precepte(tags("but in Précepte")).apply((s: ST[Int]) => Future(0))
    // val none = Precepte(tags("none")).apply((s: ST[Int]) => Future(0))
    // val is = Precepte(tags("is")).apply((s: ST[Int]) => Future(0))
    // val aspureas = Precepte(tags("as pure as")).apply((s: ST[Int]) => Future(0))
    // val kats = Precepte(tags("cats")).apply((s: ST[Int]) => Future(0))
    // val animal = Precepte(tags("animal")).apply((s: ST[Int]) => Future(0))
    // val framework = Precepte(tags("framework")).apply((s: ST[Int]) => Future(0))
    // val purely = Precepte(tags("purely")).apply((s: ST[Int]) => Future(0))
    // val functional = Precepte(tags("functional")).apply((s: ST[Int]) => Future(0))
    // val typesafe = Precepte(tags("typesafe")).apply((s: ST[Int]) => Future(0))
    // val api = Precepte(tags("api")).apply((s: ST[Int]) => Future(0))
    // val theEnd = Precepte(tags("the end")).apply((s: ST[Int]) => Future(0))

    // val p9 = (Pre(tags("persian")).apply((s: ST[Int]) => Future(2))
    //                     |@| Pre(tags("main coon")).apply((s: ST[Int]) => Future(2))).tupled 
    // val p8 =
    //   for {
    //     _ <-  Pre(tags("cats")).apply((s: ST[Int]) => Future(0))
    //     _ <-  Pre(tags("can be"))(p9)
                
    //     // _ <- but
    //     // _ <- kats
    //     // _ <- is
    //     // _ <- (purely |@| functional |@| typesafe |@| api).tupled
    //     // _ <- theEnd
    //   } yield ()

    val p0 = Precepte(tags("p0")).applyU((s: ST[Int]) => Future(0 -> 0))
    val p1 = Precepte(tags("p1")).applyU((s: ST[Int]) => Future(1 -> 1))
    val p2 = Precepte(tags("p2")).applyU((s: ST[Int]) => Future(2 -> 2))
    val p3 = Precepte(tags("p3")).applyU((s: ST[Int]) => Future(3 -> 3))
    val p4 = Precepte(tags("p4")).applyU((s: ST[Int]) => Future(4 -> 4))
    val p5 = Precepte(tags("p5")).applyU((s: ST[Int]) => Future(5 -> 5))
    val p6 = Precepte(tags("p6")).applyU((s: ST[Int]) => Future(6 -> 6))
    val p7 = Precepte(tags("p7")).applyU((s: ST[Int]) => Future(7 -> 7))

    val p8 =
      for {
      // _ <- p0
      // _ <- (p1 |@| p2 |@| p3).tupled
      _ <- Precepte(tags("sub"))(p4)
      // _ <- p5
      // _ <- Precepte(tags("sub2"))(for {
      //     _ <- (p1 |@| p2 |@| p3 |@| p4).tupled
      //     _ <- p6
      //     _ <- (p4 |@| p5 |@| Precepte(tags("sub3"))(p6)).tupled
      //     _ <- p7
      //   } yield ())
    } yield ()


    val (ssubap2, _) =
      p8
        .graph(Graph.empty)
        .eval(nostate)
        .futureValue
    println(ssubap2.viz)

  }
}