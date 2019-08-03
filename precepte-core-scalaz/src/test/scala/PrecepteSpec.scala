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
package corescalaz

import org.scalatest._
import Matchers._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

class PrecepteSpec extends FlatSpec with ScalaFutures with Inside {

  implicit val defaultPatience =
    PatienceConfig(timeout = TSpan(300, Seconds), interval = TSpan(5, Millis))

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import scalaz.std.scalaFuture._
  import scalaz.syntax.monad._

  import default._

  type Pre[A] = DefaultPre[Future, Unit, A]

  val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))

  private def tags(n: String) = BaseTags(Callee(n), Category.Database)

  def nostate = ST(Span.gen, env, Vector.empty, ())

  implicit val unitSG = new scalaz.Semigroup[Unit] {
    def append(f1: Unit, f2: => Unit) = ()
  }

  val ids = EndlessStream.unfold(1L)(n => (PId(n.toString), n + 1))

  "Precepte" should "run/eval simple" in {
    def f1 = Precepte(tags("simple.f1")) { (_: ST[Unit]) =>
      1.point[Future]
    }
    def f2(i: Int) = Precepte(tags("simple.f2")) { (_: ST[Unit]) =>
      s"foo $i".point[Future]
    }

    val res = for {
      i <- f1
      r <- f2(i)
    } yield r

    val (s, a) = res.run(nostate).futureValue
    a should ===("foo 1")

    s.managed.env should ===(env)
    s.managed.path.size should ===(2)
    s.managed.path(0).tags should ===(tags("simple.f1"))
    s.managed.path(1).tags should ===(tags("simple.f2"))

    val a0 = res.eval(nostate).futureValue
    a0 should ===("foo 1")
  }

  it should "be able to create Precepte from pure value" in {
    val p: Pre[Int] = Precepte.pure(5)

    p.eval(nostate).futureValue should equal(5)
  }

  it should "observe simple" in {
    def f1 =
      Precepte(tags("simple.f1")) { (_: ST[Unit]) =>
        1.point[Future]
      }.flatMap(a =>
          Precepte(tags("simple.f1.1")) { (_: ST[Unit]) =>
            (a + 1).point[Future]
        })
        .flatMap(a =>
          Precepte(tags("simple.f1.2")) { (_: ST[Unit]) =>
            (a + 1).point[Future]
        })
        .flatMap(a =>
          Precepte(tags("simple.f1.3")) { (_: ST[Unit]) =>
            (a + 1).point[Future]
        })
    def f2(i: Int) =
      Precepte(tags("simple.f2")) { (_: ST[Unit]) =>
        s"foo $i".point[Future]
      }.flatMap(a =>
          Precepte(tags("simple.f2.1")) { (_: ST[Unit]) =>
            s"$a.1".point[Future]
        })
        .flatMap(a =>
          Precepte(tags("simple.f2.2")) { (_: ST[Unit]) =>
            s"$a.2".point[Future]
        })
    def f3(s: String) =
      Precepte(tags("simple.f3")) { (_: ST[Unit]) =>
        s"$s finito".point[Future]
      }

    val res = Precepte(tags("root")) {
      for {
        i <- f1
        s <- f2(i)
        r <- f3(s)
      } yield r
    }

    val (s, a) = res.run(nostate).futureValue
    a should ===("foo 4.1.2 finito")
    s.managed.path(0).tags.callee should ===(Callee("root"))
    s.managed.path(0).tags.category should ===(Category.Database)

    s.managed.path(1).tags.callee should ===(Callee("simple.f1"))

    s.managed.path.map(_.tags.callee.value) should equal(
      Vector("root",
             "simple.f1",
             "simple.f1.1",
             "simple.f1.2",
             "simple.f1.3",
             "simple.f2",
             "simple.f2.1",
             "simple.f2.2",
             "simple.f3")
    )
  }

  it should "OptT" in {
    val f1 = Precepte(tags("opt"))((_: ST[Unit]) => Option("foo").point[Future])
    val f2 = Precepte(tags("opt"))((_: ST[Unit]) => Option(1).point[Future])
    val f3 =
      Precepte(tags("opt"))((_: ST[Unit]) => (None: Option[Int]).point[Future])

    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.run.eval(nostate).futureValue should ===(Some(("foo", 1)))

    val res2 = for {
      e1 <- trans(f1)
      e2 <- trans(f3)
    } yield (e1, e2)

    res2.run.eval(nostate).futureValue should ===(None)

    val res3 = for {
      e1 <- trans(f3)
      e2 <- trans(f2)
    } yield (e1, e2)

    res3.run.eval(nostate).futureValue should ===(None)
  }

  it should "ListT" in {
    val f1 =
      Precepte(tags("listT"))((_: ST[Unit]) => List("foo", "bar").point[Future])
    val f2 = Precepte(tags("listT"))((_: ST[Unit]) => List(1, 2).point[Future])
    val f3 = Precepte(tags("listT"))((_: ST[Unit]) => List[Int]().point[Future])

    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.run.eval(nostate).futureValue should ===(
      List(("foo", 1), ("foo", 2), ("bar", 1), ("bar", 2)))

    val res2 = for {
      e1 <- trans(f1)
      e2 <- trans(f3)
    } yield (e1, e2)

    res2.run.eval(nostate).futureValue should ===(List())

    val res3 = for {
      e1 <- trans(f3)
      e2 <- trans(f2)
    } yield (e1, e2)

    res3.run.eval(nostate).futureValue should ===(List())
  }

  it should "EitherT" in {
    import scalaz.{\/, \/-, -\/}

    val f1: Pre[String \/ String] =
      Precepte(tags("f1"))(_ => \/-("foo").point[Future])
    val f2: Pre[String \/ Int] =
      Precepte(tags("f2"))(_ => \/-(1).point[Future])
    val f3: Pre[String \/ String] =
      Precepte(tags("f3"))(_ => -\/("Error").point[Future])

    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.run.eval(nostate).futureValue should ===(\/-("foo" -> 1))

    val error = -\/("Error")
    val res2 = for {
      e1 <- trans(f1)
      e2 <- trans(f3)
    } yield (e1, e2)

    res2.run.eval(nostate).futureValue should ===(error)

    val res3 = for {
      e1 <- trans(f3)
      e2 <- trans(f2)
    } yield (e1, e2)

    val rr0 = res3.run.eval(nostate).futureValue
    rr0 should ===(error)

  }

  it should "pass context" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[ST[Unit]]()

    def push(state: ST[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")) { (c: ST[Unit]) =>
      push(c)
      1.point[Future]
    }

    val res0 = f1.eval(nostate).futureValue
    res0 should ===(1)
    ctxs.length should ===(1)

  }

  it should "preserve context on map" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[ST[Unit]]()

    def push(state: ST[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 =
      Precepte(tags("f1")) { (c: ST[Unit]) =>
        push(c)
        1.point[Future]
      }.map(identity).map(identity).map(identity).map(identity)

    val res0 = f1.eval(nostate).futureValue
    res0 should ===(1)
    ctxs.length should ===(1)

  }

  it should "preserve context on flatMap" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[ST[Unit]]()

    def push(state: ST[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")) { (c: ST[Unit]) =>
      push(c)
      1.point[Future]
    }

    def f2(i: Int) = Precepte(tags("f2")) { (c: ST[Unit]) =>
      push(c)
      s"foo $i".point[Future]
    }

    def f3(s: String) = Precepte(tags("f3")) { (c: ST[Unit]) =>
      push(c)
      s"f3 $s".point[Future]
    }

    val f = Precepte(tags("anon0"))(
      f1.flatMap(i => f2(i))
        .flatMap(s => f3(s)))

    val res = f.eval(nostate).futureValue
    res should ===("f3 foo 1")

    ctxs.length should ===(3)

  }

  it should "stack contexts" in {
    def f1 = Precepte(tags("f1")) { (_: ST[Unit]) =>
      1.point[Future]
    }

    val stacked = Precepte(tags("stacked"))(f1)
    val r = stacked.eval(nostate).futureValue
    r should ===(1)

  }

  it should "provide context to C" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[ST[Unit]]()

    def push(state: ST[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")) { (c: ST[Unit]) =>
      push(c)
      1.point[Future]
    }

    def f2(i: Int) = Precepte(tags("f2")) { (c: ST[Unit]) =>
      push(c)
      s"foo $i".point[Future]
    }

    val r = for {
      i <- f1
      r <- f2(i)
    } yield r

    r.eval(nostate).futureValue should ===("foo 1")

    ctxs should have length (2)
    ctxs.map(_.managed.span).toSet.size should ===(1) // span is unique
    ctxs(0).managed.path.length should ===(1)
    ctxs(1).managed.path.length should ===(2)

  }

  it should "not stack context on trans" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[ST[Unit]]()

    def push(state: ST[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")) { (c: ST[Unit]) =>
      push(c)
      Option(1).point[Future]
    }

    def f2(i: Int) = Precepte(tags("f1")) { (c: ST[Unit]) =>
      push(c)
      Option(s"foo $i").point[Future]
    }

    val res4 = Precepte(tags("res4")) {
      (for {
        i <- trans(f1)
        r <- trans(f2(i))
      } yield r).run
    }

    res4.eval(nostate).futureValue should ===(Some("foo 1"))

    ctxs should have length (2)
    ctxs.map(_.managed.span).toSet.size should ===(1) // span is unique
    ctxs(0).managed.path.length should ===(2)
    ctxs(1).managed.path.length should ===(3)

  }

  it should "not break type inference" in {
    import scalaz.syntax.monadPlus._
    import scalaz.OptionT._

    val f1 = Option(1).point[Pre]

    optionT(f1)
      .withFilter(_ => true)
      .withFilter(_ => true)
      .run
      .eval(nostate)
      .futureValue should ===(Some(1))

  }

  it should "not stack overflow" in {
    def pre(l: List[Int], i: Int) =
      Precepte(tags(s"stack_$i"))((_: ST[Unit]) => l.point[Future])

    val l = List.iterate(0, 100000) { i =>
      i + 1
    }

    val pf = l.foldLeft(
      pre(List(), 0)
    ) {
      case (p, i) =>
        p.flatMap(l => pre(i +: l, i))
    }

    pf.eval(nostate).futureValue should equal(l.reverse)
  }

  it should "observe" in {

    implicit val intSG = new scalaz.Semigroup[Int] {
      def append(f1: Int, f2: => Int) = f1 + f2
    }

    def nostate = ST(Span.gen, env, Vector.empty, 0)

    // type Pre[A] = DefaultPre[Future, Int, A]

    // object Pre extends DefaultPreBuilder[Future, Int, Pre]

    val p0 = Precepte(tags("p0")).applyU((_: ST[Int]) => Future(0 -> 0))
    val p1 = Precepte(tags("p1")).applyU((_: ST[Int]) => Future(1 -> 1))
    val p2 = Precepte(tags("p2")).applyU((_: ST[Int]) => Future(2 -> 2))
    val p3 = Precepte(tags("p3")).applyU((_: ST[Int]) => Future(3 -> 3))
    val p4 = Precepte(tags("p4")).applyU((_: ST[Int]) => Future(4 -> 4))
    val p5 = Precepte(tags("p5")).applyU((_: ST[Int]) => Future(5 -> 5))
    val p6 = Precepte(tags("p6")).applyU((_: ST[Int]) => Future(6 -> 6))
    val p7 = Precepte(tags("p7")).applyU((_: ST[Int]) => Future(7 -> 7))

    val p8 =
      for {
        _ <- p0
        _ <- (p1 |@| p2 |@| p3).tupled
        _ <- Precepte(tags("sub"))(p4)
        _ <- p5
        _ <- Precepte(tags("sub2"))(for {
          _ <- (p1 |@| p2 |@| p3 |@| p4).tupled
          _ <- p6
          _ <- (p4 |@| p5 |@| Precepte(tags("sub3"))(p6)).tupled
          _ <- p7
        } yield ())
      } yield ()

    val (s, _) =
      Precepte(tags("subzero")) { (s: ST[Int]) =>
        p4.graph(Graph.empty).eval(s).map {
          case (g, a) =>
            val value = s.managed.path.last.tags.callee.value
            val id = value + "_" + s.managed.path.last.id.value
            val g1 = Graph.empty + Sub(id, value, g)
            (g1, a)
        }
      }.eval(nostate).futureValue

    println("=== s ===")
    println(s.viz)

    println("=== (p1 |@| p2) ===")
    val (s1, _) =
      (p1 |@| p2).tupled.graph(Graph.empty).eval(nostate).futureValue
    println(s1.viz)

    println("=== s2 ===")
    val (s2, _) = Precepte(tags("sub"))(p4.flatMap(_ => p1))
      .graph(Graph.empty)
      .eval(nostate)
      .futureValue
    println(s2.viz)

    println("=== (p1 |@| p2 |@| p3) flatMap p4 ===")
    val ptest =
      for {
        _ <- (p1 |@| p2 |@| p3).tupled
        _ <- p4
      } yield ()
    val (s3, _) = ptest.graph(Graph.empty).eval(nostate).futureValue
    println(s3.viz)

    println("=== pX ===")
    val px =
      for {
        _ <- p0
        _ <- (p1 |@| p2 |@| p3).tupled
        _ <- Precepte(tags("sub"))(p4)
        _ <- p5
      } yield ()

    val (sx, _) = px.graph(Graph.empty).eval(nostate).futureValue
    println(sx.viz)

    println("=== psubap ===")
    // val psubap = Precepte(tags("sub"))((p1 |@| p2).tupled)
    val psubap = Precepte(tags("sub"))(p1.map(identity))
    val (ssubap, _) = psubap.graph(Graph.empty).eval(nostate).futureValue
    println(ssubap.viz)

    println("=== psubap2 ===")
    val (ssubap2, _) =
      p8.graph(Graph.empty)
        .eval(nostate)
        .futureValue
    println(ssubap2.viz)

    println("=== simple ===")
    val (sp1, _) =
      Precepte(tags("sub"))(p1).graph(Graph.empty).eval(nostate).futureValue
    println(sp1.viz)

    1 should ===(1)
  }

  it should "compile using nat" in {
    final case class F[A](a: A)
    def f1 = Precepte(tags("f1")) { (_: ST[Unit]) =>
      F(1)
    }
    def f2 = Precepte(tags("f2")) { (_: ST[Unit]) =>
      F(2)
    }
    def f3 = Precepte(tags("f3")) { (_: ST[Unit]) =>
      F(3)
    }
    def f4(i: Int) = Precepte(tags("f4")) { (_: ST[Unit]) =>
      F(s"$i")
    }

    val res = for {
      i <- f1
      a <- (f2 |@| f3).tupled
      r <- f4(i + a._1 + a._2)
    } yield r

    import scala.concurrent.duration._
    import scalaz.~>

    val nat = new scalaz.Isomorphism.<~>[F, Future] {
      def to: F ~> Future = new (F ~> Future) {
        def apply[A](f: F[A]): Future[A] = f.a.point[Future]
      }
      def from: Future ~> F = new (Future ~> F) {
        def apply[A](f: Future[A]): F[A] =
          F(scala.concurrent.Await.result(f, 10.seconds))
      }
    }

    val a0 = new ScalazExt(res).compile(nat).eval(nostate).futureValue
    a0 should ===("6")
  }

  it should "mapSuspension" in {
    val p0 = Precepte(tags("p0")).applyU((_: ST[Int]) => Future(0 -> 0))
    val p1 = Precepte(tags("p1")).applyU((_: ST[Int]) => Future(1 -> 1))
    val p2 = Precepte(tags("p2")).applyU((_: ST[Int]) => Future(2 -> 2))
    val p3 = Precepte(tags("p3")).applyU((_: ST[Int]) => Future(3 -> 3))
    val p4 = Precepte(tags("p4")).applyU((_: ST[Int]) => Future(4 -> 4))
    val p5 = Precepte(tags("p5")).applyU((_: ST[Int]) => Future(5 -> 5))
    val p6 = Precepte(tags("p6")).applyU((_: ST[Int]) => Future(6 -> 6))
    val p7 = Precepte(tags("p7")).applyU((_: ST[Int]) => Future(7 -> 7))

    val p8 =
      for {
        _ <- p0
        _ <- (p1 |@| p2 |@| p3).tupled
        _ <- Precepte(tags("sub"))(p4)
        _ <- p5
        _ <- Precepte(tags("sub2"))(for {
          _ <- (p1 |@| p2 |@| p3 |@| p4).tupled
          _ <- p6
          _ <- (p4 |@| p5 |@| Precepte(tags("sub3"))(p6)).tupled
          _ <- p7
        } yield ())
      } yield ()

    import scalaz.~>

    type P[X] = Precepte[BaseTags, MS, Int, Future, X]
    object P extends Precepte.API[BaseTags, MS, Int, Future]

    object Mon extends (P ~> P) {
      def apply[A](f: P[A]): P[A] =
        for {
          state0 <- P.get
          t0 <- P.delay(System.nanoTime())
          a <- f
          t1 <- P.delay(System.nanoTime())
          _ <- P.delay {

            val path = state0.managed.path
            val method = path.last.tags.callee.value
            val duration = t1 - t0

            // TODO: Store measure and test result
            println(s"$method $duration")
          }
        } yield a
    }

    implicit val intSG = new scalaz.Semigroup[Int] {
      def append(f1: Int, f2: => Int) = f1 + f2
    }
    def nostate = ST(Span.gen, env, Vector.empty, 0)

    val res = new ScalazExt(p8).mapSuspension(Mon).eval(nostate).futureValue
    res should ===(())
  }

}
