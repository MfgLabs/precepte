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
package corecats

import org.scalatest._
import Matchers._
import Inspectors._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{ Millis, Seconds, Span => TSpan }

import scala.language.higherKinds


class PrecepteSpec extends FlatSpec with ScalaFutures with Inside {

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

  def nostate = ST(Span.gen, env, Vector.empty, ())

  implicit val unitSG = new cats.Semigroup[Unit] {
    def combine(f1: Unit, f2: Unit) = ()
  }

  val ids = PIdStream((1 to 30).map(i => PId(i.toString)).toStream)

  "Precepte" should "run/eval simple" in {
    def f1 = Precepte(tags("simple.f1")){(_: ST[Unit]) => Applicative[Future].pure(1)}
    def f2(i: Int) = Precepte(tags("simple.f2")){(_: ST[Unit]) => Applicative[Future].pure(s"foo $i")}

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
    val p: Pre[Int] = Pre.pure(5)

    p.eval(nostate).futureValue should equal (5)
  }

  it should "observe simple" in {
    def f1 =
      Precepte(tags("simple.f1")){(_: ST[Unit]) => Applicative[Future].pure(1)}
        .flatMap(a => Precepte(tags("simple.f1.1")){(_: ST[Unit]) => Applicative[Future].pure(a+1)})
        .flatMap(a => Precepte(tags("simple.f1.2")){(_: ST[Unit]) => Applicative[Future].pure(a+1)})
        .flatMap(a => Precepte(tags("simple.f1.3")){(_: ST[Unit]) => Applicative[Future].pure(a+1)})
    def f2(i: Int) =
      Precepte(tags("simple.f2")){(_: ST[Unit]) => Applicative[Future].pure(s"foo $i")}
        .flatMap(a => Precepte(tags("simple.f2.1")){(_: ST[Unit]) => Applicative[Future].pure(s"$a.1")})
        .flatMap(a => Precepte(tags("simple.f2.2")){(_: ST[Unit]) => Applicative[Future].pure(s"$a.2")})
    def f3(s: String) =
      Precepte(tags("simple.f3")){(_: ST[Unit]) => Applicative[Future].pure(s"$s finito")}

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

    s.managed.path.map(_.tags.callee.value) should equal (
      Vector("root", "simple.f1", "simple.f1.1", "simple.f1.2", "simple.f1.3", "simple.f2", "simple.f2.1", "simple.f2.2", "simple.f3")
    )
  }


  it should "OptT" in {
    val f1 = Precepte(tags("opt"))((_: ST[Unit]) => Applicative[Future].pure(Option("foo")))
    val f2 = Precepte(tags("opt"))((_: ST[Unit]) => Applicative[Future].pure(Option(1)))
    val f3 = Precepte(tags("opt"))((_: ST[Unit]) => Applicative[Future].pure((None: Option[Int])))


    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.value.eval(nostate).futureValue should ===(Some(("foo",1)))

    val res2 = for {
      e1 <- trans(f1)
      e2 <- trans(f3)
    } yield (e1, e2)

    res2.value.eval(nostate).futureValue should ===(None)

    val res3 = for {
      e1 <- trans(f3)
      e2 <- trans(f2)
    } yield (e1, e2)

    res3.value.eval(nostate).futureValue should ===(None)
  }


  // it should "StreamingT" in {
  //   val f1 = Precepte(tags("StreamingT1"))((_: ST[Unit]) => Applicative[Future].pure(StreamingT("foo", "bar")))
  //   val f2 = Precepte(tags("StreamingT2"))((_: ST[Unit]) => Applicative[Future].pure(StreamingT(1, 2)))
  //   val f3 = Precepte(tags("StreamingT3"))((_: ST[Unit]) => Applicative[Future].pure(StreamingT.empty[Int]))

  //   val res = for {
  //     e1 <- trans(f1)
  //     e2 <- trans(f2)
  //   } yield (e1, e2)

  //   res.run.eval(nostate).futureValue should ===(List(("foo",1), ("foo",2), ("bar",1), ("bar",2)))

  //   val res2 = for {
  //     e1 <- trans(f1)
  //     e2 <- trans(f3)
  //   } yield (e1, e2)

  //   res2.run.eval(nostate).futureValue should ===(List())

  //   val res3 = for {
  //     e1 <- trans(f3)
  //     e2 <- trans(f2)
  //   } yield (e1, e2)

  //   res3.run.eval(nostate).futureValue should ===(List())
  // }


  it should "EitherT" in {
    import cats.data.Xor
    // import cata.data.EitherT.eitherTFunctor

    val f1: Pre[Xor[String, String]] =
      Precepte(tags("f1"))(_ => Applicative[Future].pure(Xor.Right("foo")))
    val f2: Pre[Xor[String, Int]] =
      Precepte(tags("f2"))(_ => Applicative[Future].pure(Xor.Right(1)))
    val f3: Pre[Xor[String, String]] =
      Precepte(tags("f3"))(_ => Applicative[Future].pure(Xor.Left("Error")))

    type Foo[A] = XorT[Future, String, A]

    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.value.eval(nostate).futureValue should ===(Xor.Right("foo" -> 1))

    val error = Xor.Left("Error")
    val res2 = for {
      e1 <- trans(f1)
      e2 <- trans(f3)
    } yield (e1, e2)

    res2.value.eval(nostate).futureValue should ===(error)

    val res3 = for {
      e1 <- trans(f3)
      e2 <- trans(f2)
    } yield (e1, e2)

    val rr0 = res3.value.eval(nostate).futureValue
    rr0 should ===(error)

  }



  it should "pass context" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[ST[Unit]]()

    def push(state: ST[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")){ (c: ST[Unit]) =>
      push(c)
      Applicative[Future].pure(1)
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

    def f1 = Precepte(tags("f1")){ (c: ST[Unit]) =>
      push(c)
      Applicative[Future].pure(1)
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

    def f1 = Precepte(tags("f1")){ (c: ST[Unit]) =>
      push(c)
      Applicative[Future].pure(1)
    }

    def f2(i: Int) = Precepte(tags("f2")){ (c: ST[Unit]) =>
      push(c)
      Applicative[Future].pure(s"foo $i")
    }

    def f3(s: String) = Precepte(tags("f3")){ (c: ST[Unit]) =>
      push(c)
      Applicative[Future].pure(s"f3 $s")
    }

    val f = Precepte(tags("anon0"))(f1
      .flatMap(i => f2(i))
      .flatMap(s => f3(s)))

    val res = f.eval(nostate).futureValue
    res should ===("f3 foo 1")

    ctxs.length should ===(3)

  }

  it should "stack contexts" in {
    def f1 = Precepte(tags("f1")){ (c: ST[Unit]) =>
      Applicative[Future].pure(1)
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
      Applicative[Future].pure(1)
    }

    def f2(i: Int) = Precepte(tags("f2")){ (c: ST[Unit]) =>
      push(c)
      Applicative[Future].pure(s"foo $i")
    }

    val r = for {
      i <- f1
      r <- f2(i)
    } yield r

    r.eval(nostate).futureValue should ===("foo 1")

    ctxs should have length(2)
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
      Applicative[Future].pure(Option(1))
    }

    def f2(i: Int) = Precepte(tags("f1")){ (c: ST[Unit]) =>
      push(c)
      Applicative[Future].pure(Option(s"foo $i"))
    }

    val res4 = Precepte(tags("res4")) {
      (for {
        i <- trans(f1)
        r <- trans(f2(i))
      } yield r).value
    }

    res4.eval(nostate).futureValue should ===(Some("foo 1"))

    ctxs should have length(2)
    ctxs.map(_.managed.span).toSet.size should ===(1) // span is unique
    ctxs(0).managed.path.length should ===(2)
    ctxs(1).managed.path.length should ===(3)

  }

  it should "not break type inference" in {
    // import cats.syntax.monadPlus._
    // import cats.syntax.Ops
    import cats.data.OptionT
    // import cats.syntax.monadFilter._
    // import cats._
    import scala.language.implicitConversions

    val f1 = Applicative[Pre].pure(Option(1))

    OptionT(f1).filter(_ => true).filter(_ => true).value.eval(nostate).futureValue should ===(Some(1))

  }

  it should "not stack overflow" in {
    def pre(l: List[Int], i: Int) = Precepte(tags(s"stack_$i"))((_: ST[Unit]) => Applicative[Future].pure(l))

    val l = List.iterate(0, 100000){ i => i + 1 }

    val pf = l.foldLeft(
      pre(List(), 0)
    ){ case (p, i) =>
      p.flatMap(l => pre(i +: l, i))
    }

    pf.eval(nostate).futureValue should equal (l.reverse)
  }

  it should "observe" in {

    implicit val intSG = new cats.Semigroup[Int] {
      def combine(f1: Int, f2: Int) = f1 + f2
    }

    def nostate = ST(Span.gen, env, Vector.empty, 0)

    // type Pre[A] = DefaultPre[Future, Int, A]

    // object Pre extends DefaultPreBuilder[Future, Int, Pre]

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
        p4.graph(Graph.empty).eval(s).map { case (g, a) =>
          val value = s.managed.path.last.tags.callee.value
          val id = value + "_" + s.managed.path.last.id.value
          val g1 = Graph.empty + Sub(id, value, g)
          (g1, a)
        }
      }.eval(nostate).futureValue

    // println("=== s ===")
    // println(s.viz)

    // println("=== (p1 |@| p2) ===")
    val (s1, _) = (p1 |@| p2).tupled.graph(Graph.empty).eval(nostate).futureValue
    // println(s1.viz)

    // println("=== s2 ===")
    val (s2, _) = Precepte(tags("sub"))(p4.flatMap(_ => p1)).graph(Graph.empty).eval(nostate).futureValue
    // println(s2.viz)

    // println("=== (p1 |@| p2 |@| p3) flatMap p4 ===")
    val ptest =
      for {
        _ <- (p1 |@| p2 |@| p3).tupled
        _ <- p4
      } yield ()
    val (s3, _) = ptest.graph(Graph.empty).eval(nostate).futureValue
    // println(s3.viz)

    // println("=== pX ===")
    val px =
      for {
      _ <- p0
      _ <- (p1 |@| p2 |@| p3).tupled
      _ <- Precepte(tags("sub"))(p4)
      _ <- p5
    } yield ()

    val (sx, _) = px.graph(Graph.empty).eval(nostate).futureValue
    // println(sx.viz)

    // println("=== psubap ===")
    val psubap = Precepte(tags("sub"))(p1.map(identity))
    val (ssubap, _) = psubap.graph(Graph.empty).eval(nostate).futureValue
    // println(ssubap.viz)

    // println("=== psubap2 ===")
    val (ssubap2, _) =
      p8
        .graph(Graph.empty)
        .eval(nostate)
        .futureValue
    // println(ssubap2.viz)

    // println("=== simple ===")
    val (sp1, _) =
      Precepte(tags("sub"))(p1).graph(Graph.empty).eval(nostate).futureValue
    // println(sp1.viz)

    1 should ===(1)
  }

  it should "compile using nat" in {
    case class F[A](a: A)
    def f1 = Precepte(tags("f1")){ (_: ST[Unit]) => F(1) }
    def f2 = Precepte(tags("f2")){ (_: ST[Unit]) => F(2) }
    def f3 = Precepte(tags("f3")){ (_: ST[Unit]) => F(3) }
    def f4(i: Int) = Precepte(tags("f4")){ (_: ST[Unit]) => F(s"$i") }

    val res = for {
      i <- f1
      a <- (f2 |@| f3).tupled
      r <- f4(i + a._1 + a._2)
    } yield r

    val nat1 = new cats.~>[F, Future] {
      def apply[A](f: F[A]): Future[A] = Applicative[Future].pure(f.a)
    }

    import scala.concurrent.Await
    import scala.concurrent.duration._

    val nat2 = new cats.~>[Future, F] {
      def apply[A](f: Future[A]): F[A] = F(Await.result(f, 10.seconds))
    }

    val a0 = CatsExt(res).compile(nat1, nat2).eval(nostate).futureValue
    a0 should ===("6")
  }


  it should "mapSuspension" in {
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

    import cats.~>

    type SF[T] = (ST[Int], Future[T])

    object Mon extends (SF ~> Future) {
      def apply[A](f: SF[A]): Future[A] = {
        val t0 = System.nanoTime()
        val path = f._1.managed.path
        val method = path.last.tags.callee.value
        f._2.map { a =>
          val t1 = System.nanoTime()
          val duration = t1 - t0
          // TODO: Store measure and test result
          // println(s"$method $duration")
          a
        }
      }
    }

    implicit val intSG = new cats.Semigroup[Int] {
      def combine(f1: Int, f2: Int) = f1 + f2
    }
    def nostate = ST(Span.gen, env, Vector.empty, 0)

    val res = CatsExt(p8).mapSuspension(Mon).eval(nostate).futureValue
    res should ===(())
  }

  it should "mapF" in {
    val success = Applicative[Pre].pure("YOUPI")
    val ex = new RuntimeException("opps")
    val fail = Future.failed[String](ex)
    val failure = Precepte(tags("fail"))((_: ST[Unit]) => fail)

    val p1 =
      for {
        s0 <- success
        s1 <- success
        s2 <- success
      } yield s"$s0 $s1 $s2"

    p1.eval(nostate).futureValue should ===("YOUPI YOUPI YOUPI")

    val reco1 =
      for {
        s <- p1
        r <- failure.mapF{ (s, fut) => fut.recover{ case e => "YOLO" } }
      } yield s"$s $r"

    reco1.eval(nostate).futureValue should ===("YOUPI YOUPI YOUPI YOLO")

    val reco2 =
      (for {
        s <- p1
        r <- failure
      } yield s"$s $r").mapF{ (s, fut) => fut.recover{ case e => "YOLO" } }

    reco2.eval(nostate).futureValue should ===("YOLO")
  }

}
