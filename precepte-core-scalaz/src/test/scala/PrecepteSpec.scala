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
  import scalaz.std.scalaFuture._
  import scalaz.syntax.monad._
  import scalaz.EitherT

  import default._

  type Pre[A] = DefaultPre[Future, Unit, A]

  val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))

  private def tags(n: String) = BaseTags(Callee(n), Category.Database)

  def nostate = ST(Span.gen, env, Vector.empty, ())

  implicit val unitSG = new scalaz.Semigroup[Unit] {
    def append(f1: Unit, f2: => Unit) = ()
  }

  val ids = PIdStream((1 to 30).map(i => PId(i.toString)).toStream)

  "Precepte" should "run/eval simple" in {
    def f1 = Precepte(tags("simple.f1")){(_: ST[Unit]) => 1.point[Future]}
    def f2(i: Int) = Precepte(tags("simple.f2")){(_: ST[Unit]) => s"foo $i".point[Future]}

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
      Precepte(tags("simple.f1")){(_: ST[Unit]) => 1.point[Future]}
        .flatMap(a => Precepte(tags("simple.f1.1")){(_: ST[Unit]) => (a+1).point[Future]})
        .flatMap(a => Precepte(tags("simple.f1.2")){(_: ST[Unit]) => (a+1).point[Future]})
        .flatMap(a => Precepte(tags("simple.f1.3")){(_: ST[Unit]) => (a+1).point[Future]})
    def f2(i: Int) =
      Precepte(tags("simple.f2")){(_: ST[Unit]) => s"foo $i".point[Future]}
        .flatMap(a => Precepte(tags("simple.f2.1")){(_: ST[Unit]) => s"$a.1".point[Future]})
        .flatMap(a => Precepte(tags("simple.f2.2")){(_: ST[Unit]) => s"$a.2".point[Future]})
    def f3(s: String) =
      Precepte(tags("simple.f3")){(_: ST[Unit]) => s"$s finito".point[Future]}

    val res = Precepte(tags("root")) {
      for {
        i <- f1
        s <- f2(i)
        r <- f3(s)
      } yield r
    }

    val (s, a) = res.run(nostate).futureValue
    a should ===("foo 4.1.2 finito")

    s.managed.path(0).tags.category should ===(Category.Database)

    s.managed.path.map(_.tags.callee.value) should equal (
      Vector("root", "simple.f1", "simple.f1.1", "simple.f1.2", "simple.f1.3", "simple.f2", "simple.f2.1", "simple.f2.2", "simple.f3")
    )
  }


  it should "OptT" in {
    val f1 = Precepte(tags("opt"))((_: ST[Unit]) => Option("foo").point[Future])
    val f2 = Precepte(tags("opt"))((_: ST[Unit]) => Option(1).point[Future])
    val f3 = Precepte(tags("opt"))((_: ST[Unit]) => (None: Option[Int]).point[Future])


    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.run.eval(nostate).futureValue should ===(Some(("foo",1)))

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
    val f1 = Precepte(tags("listT"))((_: ST[Unit]) => List("foo", "bar").point[Future])
    val f2 = Precepte(tags("listT"))((_: ST[Unit]) => List(1, 2).point[Future])
    val f3 = Precepte(tags("listT"))((_: ST[Unit]) => List[Int]().point[Future])

    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.run.eval(nostate).futureValue should ===(List(("foo",1), ("foo",2), ("bar",1), ("bar",2)))

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
    import scalaz.{ \/ , \/-, -\/}
    import EitherT.eitherTFunctor

    val f1: Pre[String \/ String] =
      Precepte(tags("f1"))(_ => \/-("foo").point[Future])
    val f2: Pre[String \/ Int] =
      Precepte(tags("f2"))(_ => \/-(1).point[Future])
    val f3: Pre[String \/ String] =
      Precepte(tags("f3"))(_ => -\/("Error").point[Future])

    type Foo[A] = EitherT[Future, String, A]

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

    def f1 = Precepte(tags("f1")){ (c: ST[Unit]) =>
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

    def f1 = Precepte(tags("f1")){ (c: ST[Unit]) =>
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

    def f1 = Precepte(tags("f1")){ (c: ST[Unit]) =>
      push(c)
      1.point[Future]
    }

    def f2(i: Int) = Precepte(tags("f2")){ (c: ST[Unit]) =>
      push(c)
      s"foo $i".point[Future]
    }

    def f3(s: String) = Precepte(tags("f3")){ (c: ST[Unit]) =>
      push(c)
      s"f3 $s".point[Future]
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

    def f2(i: Int) = Precepte(tags("f2")){ (c: ST[Unit]) =>
      push(c)
      s"foo $i".point[Future]
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
      Option(1).point[Future]
    }

    def f2(i: Int) = Precepte(tags("f1")){ (c: ST[Unit]) =>
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

    ctxs should have length(2)
    ctxs.map(_.managed.span).toSet.size should ===(1) // span is unique
    ctxs(0).managed.path.length should ===(2)
    ctxs(1).managed.path.length should ===(3)

  }

  it should "not break type inference" in {
    import scalaz.syntax.monadPlus._
    import scalaz.syntax.Ops
    import scalaz.OptionT._
    import scalaz._
    import scala.language.implicitConversions

    val f1 = Option(1).point[Pre]

    optionT(f1).withFilter(_ => true).withFilter(_ => true).run.eval(nostate).futureValue should ===(Some(1))

  }

  it should "not stack overflow" in {
    def pre(l: List[Int], i: Int) = Precepte(tags(s"stack_$i"))((_: ST[Unit]) => l.point[Future])

    val l = List.iterate(0, 100000){ i => i + 1 }

    val pf = l.foldLeft(
      pre(List(), 0)
    ){ case (p, i) =>
      p.flatMap(l => pre(i +: l, i))
    }

    pf.eval(nostate).futureValue should equal (l.reverse)
  }

  it should "observe" in {

    implicit val intSG = new scalaz.Semigroup[Int] {
      def append(f1: Int, f2: => Int) = f1 + f2
    }

    def nostate = ST(Span.gen, env, Vector.empty, 0)

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

    val gs = Graph(Set(Sub("subzero_100053_0", "subzero", Graph(Set(Leaf("p4_100054_0","p4")),Set()))),Set())
    s.viz should===(gs.viz)

    val (s1, _) = (p1 |@| p2).tupled.graph(Graph.empty).eval(nostate).futureValue
    val gs1 = Graph(Set(Leaf("p2_100055_1", "p2"), Leaf("p1_100056_1", "p1")),Set())
    s1.viz should===(gs1.viz)

    val (s2, _) = Precepte(tags("sub"))(p4.flatMap(_ => p1)).graph(Graph.empty).eval(nostate).futureValue
    val gs2 = Graph(Set(Sub("sub_100061_0", "sub", Graph(Set(Leaf("p4_100059_0", "p4"), Leaf("p1_100060_0", "p1")),Set(Edge("p4_100059_0", "p1_100060_0"))))),Set())
    s2.viz should===(gs2.viz)

    val ptest =
      for {
        _ <- (p1 |@| p2 |@| p3).tupled
        _ <- p4
      } yield ()
    val (s3, _) = ptest.graph(Graph.empty).eval(nostate).futureValue
    val gs3 = Graph(Set(Leaf("p3_100062_1", "p3"), Leaf("p2_100063_4", "p2"), Leaf("p1_100064_6", "p1"), Leaf("p4_100065_0", "p4")),Set(Edge("p3_100062_1","p4_100065_0"), Edge("p2_100063_4","p4_100065_0"), Edge("p1_100064_6","p4_100065_0")))
    // s3.viz should===(gs3.viz)

    val px =
      for {
      _ <- p0
      _ <- (p1 |@| p2 |@| p3).tupled
      _ <- Precepte(tags("sub"))(p4)
      _ <- p5
    } yield ()

    val (sx, _) = px.graph(Graph.empty).eval(nostate).futureValue
    val gx = Graph(Set(Leaf("p2_100058_4","p2"), Leaf("p0_100056_0","p0"), Sub("sub_100060_0", "sub", Graph(Set(Leaf("p4_100061_0","p4")),Set())), Leaf("p1_100059_6","p1"), Leaf("p3_100057_1","p3"), Leaf("p5_100062_0","p5")),Set(Edge("p3_100057_1","p4_100061_0"), Edge("p0_100056_0","p1_100059_6"), Edge("p0_100056_0","p2_100058_4"), Edge("p4_100061_0","p5_100062_0"), Edge("p1_100059_6","p4_100061_0"), Edge("p0_100056_0","p3_100057_1"), Edge("p2_100058_4","p4_100061_0")))
    // sx.viz should===(gx.viz)


    val psubap = Precepte(tags("sub"))(p1.map(identity))
    val (st, (ssubap, _)) = psubap.graph(Graph.empty).run(nostate).futureValue
    val gsubap = Graph(Set(Sub("sub_100069_0", "sub", Graph(Set(Leaf("p1_100068_0","p1")),Set()))),Set())
    // ssubap.viz should===(gsubap.viz)

    val (ssubap2, _) =
      p8
        .graph(Graph.empty)
        .eval(nostate)
        .futureValue
    val gsubap2 = Graph(Set(Leaf("p1_100068_6","p1"), Leaf("p5_100071_0","p5"), Leaf("p2_100067_4","p2"), Leaf("p3_100066_1","p3"), Sub("sub2_100072_0", "sub2", Graph(Set(Leaf("p4_100081_6","p4"), Leaf("p6_100077_0","p6"), Leaf("p3_100074_4","p3"), Leaf("p4_100073_2","p4"), Sub("sub3_100078_1", "sub3", Graph(Set(Leaf("p6_100079_0","p6")),Set())), Leaf("p2_100075_4","p2"), Leaf("p1_100076_6","p1"), Leaf("p5_100080_4","p5"), Leaf("p7_100082_0","p7")),Set(Edge("p6_100077_0","p4_100081_6"), Edge("p5_100080_4","p7_100082_0"), Edge("p1_100076_6","p6_100077_0"), Edge("p6_100079_0","p7_100082_0"), Edge("p6_100077_0","p5_100080_4"), Edge("p4_100081_6","p7_100082_0"), Edge("p4_100073_2","p6_100077_0"), Edge("p3_100074_4","p6_100077_0"), Edge("p2_100075_4","p6_100077_0"), Edge("p6_100077_0","p6_100079_0")))), Sub("sub_100069_0", "sub", Graph(Set(Leaf("p4_100070_0","p4")),Set())), Leaf("p0_100065_0","p0")),Set(Edge("p2_100067_4","p4_100070_0"), Edge("p5_100071_0","p3_100074_4"), Edge("p0_100065_0","p2_100067_4"), Edge("p5_100071_0","p2_100075_4"), Edge("p0_100065_0","p3_100066_1"), Edge("p5_100071_0","p4_100073_2"), Edge("p3_100066_1","p4_100070_0"), Edge("p1_100068_6","p4_100070_0"), Edge("p5_100071_0","p1_100076_6"), Edge("p0_100065_0","p1_100068_6"), Edge("p4_100070_0","p5_100071_0")))
    // ssubap2.viz should===(gsubap2.viz)

    val (sp1, _) =
      Precepte(tags("sub"))(p1).graph(Graph.empty).eval(nostate).futureValue
    val gsp1 = Graph(Set(Sub("sub_100073_0", "sub", Graph(Set(Leaf("p1_100072_0","p1")),Set()))),Set())
    // sp1.viz should===(gsp1.viz)
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

    import scala.concurrent.Await
    import scala.concurrent.duration._
    import scalaz.~>

    val nat = new scalaz.Isomorphism.<~>[F, Future] {
      def to: F ~> Future = new (F~>Future) {
        def apply[A](f: F[A]): Future[A] = f.a.point[Future]
      }
      def from: Future ~> F = new (Future~>F) {
        def apply[A](f: Future[A]): F[A] = F(scala.concurrent.Await.result(f, 10.seconds))
      }
    }

    val a0 = ScalazExt(res).compile(nat).eval(nostate).futureValue
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

    import scalaz.~>

    val calls = new scala.collection.mutable.ArrayBuffer[String]()

    type SF[T] = (ST[Int], Future[T])
    object Mon extends (SF ~> Future) {
      def apply[A](f: SF[A]): Future[A] = {
        val t0 = System.nanoTime()
        val path = f._1.managed.path
        val method = path.last.tags.callee.value
        val p = path.map(_.tags.callee.value).mkString(" / ")
        calls += p
        f._2.map { a =>
          val t1 = System.nanoTime()
          val duration = t1 - t0
          // TODO: Store measure and test result
          a
        }
      }
    }

    implicit val intSG = new scalaz.Semigroup[Int] {
      def append(f1: Int, f2: => Int) = f1 + f2
    }
    def nostate = ST(Span.gen, env, Vector.empty, 0)

    val cs =
      List(
        "p0",
        "p0 / p3",
        "p0 / p2",
        "p0 / p1",
        "p0 / p3 / sub / p4",
        "p0 / p3 / sub / p4 / p5",
        "p0 / p3 / sub / p4 / p5 / sub2 / p4",
        "p0 / p3 / sub / p4 / p5 / sub2 / p3",
        "p0 / p3 / sub / p4 / p5 / sub2 / p2",
        "p0 / p3 / sub / p4 / p5 / sub2 / p1",
        "p0 / p3 / sub / p4 / p5 / sub2 / p4 / p6",
        "p0 / p3 / sub / p4 / p5 / sub2 / p4 / p6 / sub3 / p6",
        "p0 / p3 / sub / p4 / p5 / sub2 / p4 / p6 / p5",
        "p0 / p3 / sub / p4 / p5 / sub2 / p4 / p6 / p4",
        "p0 / p3 / sub / p4 / p5 / sub2 / p4 / p6 / sub3 / p6 / p7"
      )

    val res = ScalazExt(p8).mapSuspension(Mon).eval(nostate).futureValue
    res should ===(())
    calls.toList should ===(cs)
  }

  it should "chain mapSuspension without crashing" in {
    import corescalaz._

    val p0 = Precepte(tags("p0")).applyU((s: ST[Int]) => Future(0 -> 0))

    type SF[T] = (ST[Int], Future[T])
    object Id extends (SF ~~> Future) {
      def apply[A](sf: SF[A]): Future[A] = {
        val (s, f) = sf
        f
      }
    }

    implicit val intSG = new scalaz.Semigroup[Int] {
      def append(f1: Int, f2: => Int) = f1 + f2
    }

    def nostate = ST(Span.gen, env, Vector.empty, 0)

    val res = p0
      .mapSuspension(Id)
      .mapSuspension(Id)
      .mapSuspension(Id)
      .mapSuspension(Id)
      .mapSuspension(Id)
      .eval(nostate)
      .futureValue

    res should ===(0)
  }

}
