package com.mfglabs
package precepte
package corecats

import org.scalatest._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

class CatsProcessSpec extends FlatSpec with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout = TSpan(300, Seconds), interval = TSpan(5, Millis))

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import cats.instances.future._
  import cats.syntax.apply._

  import default._

  type Pre[A] = DefaultPre[Future, Int, A]

  private def tags(n: String) = BaseTags(Callee(n), Category.Database)

  it should "observe" in {

    implicit val intSG = new cats.Semigroup[Int] {
      def combine(f1: Int, f2: Int) = f1 + f2
    }

    val pre: Pre[Unit] =
      for {
        _ <- Precepte(tags("cats")) { _: ST[Int] =>
          Future(0)
        }
        _ <- Precepte(tags("can be")) { _: ST[Int] =>
          Future(1)
        }
        _ <- (Precepte(tags("persian")) { _: ST[Int] =>
          Future(2)
        }, Precepte(tags("main coon")) { _: ST[Int] =>
          Future(3)
        }, Precepte(tags("siamese")) { _: ST[Int] =>
          Future(4)
        }, Precepte(tags("sphynx")) { _: ST[Int] =>
          Future(5)
        }, Precepte(tags("burmese")) { _: ST[Int] =>
          Future(6)
        }, Precepte(tags("munchkin")) { _: ST[Int] =>
          Future(7)
        }, Precepte(tags("tabby")) { _: ST[Int] =>
          Future(8)
        }, Precepte(tags("himalayan")) { _: ST[Int] =>
          Future(9)
        }).tupled
        _ <- Precepte(tags("but")) { _: ST[Int] =>
          Future(10)
        }
        _ <- Precepte(tags("none is")) { _: ST[Int] =>
          Future(11)
        }
        _ <- Precepte(tags("as")) { _: ST[Int] =>
          Future(12)
        }
        _ <- (Precepte(tags("purely")) { _: ST[Int] =>
          Future(13)
        }, Precepte(tags("functional")) { _: ST[Int] =>
          Future(14)
        }, Precepte(tags("typesafe")) { _: ST[Int] =>
          Future(15)
        }).tupled
        _ <- Precepte(tags("as")) { _: ST[Int] =>
          Future(16)
        }
        _ <- (Precepte(tags("cats")) { _: ST[Int] =>
          Future(17)
        }, Precepte(tags("with")) { _: ST[Int] =>
          Future(18)
        }, Precepte(tags("précepte")) { _: ST[Int] =>
          Future(19)
        }).tupled
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
