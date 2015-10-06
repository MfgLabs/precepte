package com.mfglabs
package precepte
package corescalaz

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
  import scalaz.std.scalaFuture._
  import scalaz.syntax.monad._
  import scalaz.{EitherT, Semigroup}

  import default._

  type Pre[A] = DefaultPre[Future, Unit, A]

  val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))

  private def tags(n: String) = BaseTags(Callee(n), Category.Database)

  it should "observe" in {
    def nostate = ST(Span.gen, env, Vector.empty, 0)

    implicit val intSG = new Semigroup[Int] {
      def append(f1: Int, f2: => Int) = f1 + f2
    }

    val canbe = Precepte(tags("can be")).apply((s: ST[Int]) => Future(1))
    val persian = Precepte(tags("persian")).apply((s: ST[Int]) => Future(0))
    val meets = Precepte(tags("meets")).apply((s: ST[Int]) => Future(1))
    val sphynx = Precepte(tags("sphynx")).apply((s: ST[Int]) => Future(2))
    val gives = Precepte(tags("gives")).apply((s: ST[Int]) => Future(2))
    val mainCoon = Precepte(tags("main coon")).apply((s: ST[Int]) => Future(3))
    val burmese = Precepte(tags("burmese")).apply((s: ST[Int]) => Future(4))
    val munchkin = Precepte(tags("munchkin")).apply((s: ST[Int]) => Future(5))
    val tabby = Precepte(tags("tabby")).apply((s: ST[Int]) => Future(6))
    val himalayan = Precepte(tags("himalayan")).apply((s: ST[Int]) => Future(7))
    val but = Precepte(tags("but in Précepte")).apply((s: ST[Int]) => Future(0))
    val none = Precepte(tags("none")).apply((s: ST[Int]) => Future(0))
    val is = Precepte(tags("is")).apply((s: ST[Int]) => Future(0))
    val aspureas = Precepte(tags("as pure as")).apply((s: ST[Int]) => Future(0))
    val kats = Precepte(tags("cats")).apply((s: ST[Int]) => Future(0))
    val animal = Precepte(tags("animal")).apply((s: ST[Int]) => Future(0))
    val framework = Precepte(tags("framework")).apply((s: ST[Int]) => Future(0))
    val purely = Precepte(tags("purely")).apply((s: ST[Int]) => Future(0))
    val functional = Precepte(tags("functional")).apply((s: ST[Int]) => Future(0))
    val typesafe = Precepte(tags("typesafe")).apply((s: ST[Int]) => Future(0))
    val api = Precepte(tags("api")).apply((s: ST[Int]) => Future(0))
    val theEnd = Precepte(tags("the end")).apply((s: ST[Int]) => Future(0))

    val p8 =
      for {
        _ <-  Pre(tags("cats")).apply((s: ST[Int]) => Future(0))
        _ <-  Pre(tags("can be"))(
                for {
                  _ <- (Pre(tags("persian")).apply((s: ST[Int]) => Future(2))
                      |@| Pre(tags("persian")).apply((s: ST[Int]) => Future(2))
                      |@| Pre(tags("main coon")).apply((s: ST[Int]) => Future(2))
                      |@| Pre(tags("munchkin")).apply((s: ST[Int]) => Future(2)) 
                      |@| Pre(tags("burmese")).apply((s: ST[Int]) => Future(2))
                      |@| Pre(tags("tabby")).apply((s: ST[Int]) => Future(2))).tupled
                } yield ()
              )
        // _ <- but
        // _ <- kats
        // _ <- is
        // _ <- (purely |@| functional |@| typesafe |@| api).tupled
        // _ <- theEnd
      } yield ()

    val (ssubap2, _) =
      p8
        .graph(Graph.empty)
        .eval(nostate)
        .futureValue
    println(ssubap2.viz)

  }
}