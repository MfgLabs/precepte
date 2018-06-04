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
  import cats.instances.future._
  import cats.syntax.flatMap._
  import cats.syntax.apply._
  import cats.syntax._
  import cats.data.EitherT

  import default._

  type Pre[A] = DefaultPre[Future, Int, A]


  private def tags(n: String) = BaseTags(Callee(n), Category.Database)

  it should "observe" in {

    implicit val intSG = new cats.Semigroup[Int] {
      def combine(f1: Int, f2: Int) = f1 + f2
    }

    val pre: Pre[Unit] =
      for {
        _ <-  Pre(tags("cats")){ s:ST[Int] => Future(0) }
        _ <-  Pre(tags("can be")){ s:ST[Int] => Future(1) }
        _ <-  catsSyntaxTuple8Semigroupal[Pre, Int, Int, Int, Int, Int, Int, Int, Int]((
                Pre(tags("persian")){ s: ST[Int] => Future(2) }
                , Pre(tags("main coon")){ s: ST[Int] => Future(3) }
                , Pre(tags("siamese")){ s: ST[Int] => Future(4) }
                , Pre(tags("sphynx")){ s: ST[Int] => Future(5) }
                , Pre(tags("burmese")){ s: ST[Int] => Future(6) }
                , Pre(tags("munchkin")){ s: ST[Int] => Future(7) }
                , Pre(tags("tabby")){ s: ST[Int] => Future(8) }
                , Pre(tags("himalayan")){ s: ST[Int] => Future(9) }
              )).tupled
        _ <-  Pre(tags("but")){ s: ST[Int] => Future(10) }
        _ <-  Pre(tags("none is")){ s: ST[Int] => Future(11) }
        _ <-  Pre(tags("as")){ s: ST[Int] => Future(12) }
        _ <-  catsSyntaxTuple3Semigroupal[Pre, Int, Int, Int]((
                Pre(tags("purely")){ s: ST[Int] => Future(13) }
                , Pre(tags("functional")){ s: ST[Int] => Future(14) }
                , Pre(tags("typesafe")){ s: ST[Int] => Future(15) }
              )).tupled
        _ <-  Pre(tags("as")){ s: ST[Int] => Future(16) }
        _ <-  catsSyntaxTuple3Semigroupal[Pre, Int, Int, Int]((
                Pre(tags("cats")){ s: ST[Int] => Future(17) }
                , Pre(tags("with")){ s: ST[Int] => Future(18) }
                , Pre(tags("précepte")){ s: ST[Int] => Future(19) }
              )).tupled
      } yield ()

    val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))
    val state0 = ST(Span.gen, env, Vector.empty, 0)

    val (graph, _) = pre
        .graph(Graph.empty)
        .eval(state0)
        .futureValue
    println(graph.viz)

  }
}
