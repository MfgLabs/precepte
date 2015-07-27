// package com.mfglabs
// package precepte

// import org.scalatest._
// import Matchers._
// import Inspectors._

// import org.scalatest.concurrent.ScalaFutures
// import org.scalatest.concurrent.PatienceConfiguration.Timeout
// import org.scalatest.time.{Millis, Seconds, Span => TSpan}

// import scala.language.higherKinds


// class PrecepteScanSpec extends FlatSpec with ScalaFutures {

//   implicit val defaultPatience =
//     PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))

//   import scala.concurrent.ExecutionContext.Implicits.global
//   import scala.concurrent.Future
//   import scalaz.std.scalaFuture._
//   import scalaz.syntax.functor._
//   import scalaz.syntax.apply._
//   import default._

//   type P[A] = Pre[Future, Int, A]
//   implicitly[scalaz.Functor[Future]]
//   object P {
//     def apply[A](tags: BaseTags) = Precepte[BaseTags](tags)
//   }

//   val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))

//   private def tags(n: String) = BaseTags(Callee(n), Category.Database)

//   def nostate = ST(Span.gen, env, Vector.empty, 0)

//   val ids = PIdStream((1 to 30).map(i => PId(i.toString)).toStream)

//   implicit val intSG = new scalaz.Semigroup[Int] {
//     def append(f1: Int, f2: => Int) = f1 + f2
//   }

//   "Precepte" should "observe" in {
//     import PreQuiver._

//     val p0: P[Int] = P(tags("p0")).applyU((s: ST[Int]) => Future(0 -> 0))
//     val p1: P[Int] = P(tags("p1")).applyU((s: ST[Int]) => Future(1 -> 1))
//     val p2: P[Int] = P(tags("p2")).applyU((s: ST[Int]) => Future(2 -> 2))
//     val p3: P[Int] = P(tags("p3")).applyU((s: ST[Int]) => Future(3 -> 3))

//     val p4 = for {
//       _ <- p0
//       _ <- (p1 |@| p2).tupled
//       _ <- p3
//     } yield ()

//     val (_, _, graph) = p4.object(nostate).futureValue

//     println(quiver.viz.graphviz(graph))
    
//     1 should ===(1)
//   }

// }