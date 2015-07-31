package com.mfglabs
package precepte

import org.scalatest._
import Matchers._
import Inspectors._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import scala.language.higherKinds


class PrecepteSpec extends FlatSpec with ScalaFutures {

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
/*
  "Precepte" should "run/eval simple" in {
    def f1 = P(tags("simple.f1")){(_: ST[Unit]) => 1.point[Future]}
    def f2(i: Int) = P(tags("simple.f2")){(_: ST[Unit]) => s"foo $i".point[Future]}

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



  it should "observe simple" in {
    def f1 =
      P(tags("simple.f1")){(_: ST[Unit]) => 1.point[Future]}
        .flatMap(a => P(tags("simple.f1.1")){(_: ST[Unit]) => (a+1).point[Future]})
        .flatMap(a => P(tags("simple.f1.2")){(_: ST[Unit]) => (a+1).point[Future]})
        .flatMap(a => P(tags("simple.f1.3")){(_: ST[Unit]) => (a+1).point[Future]})
    def f2(i: Int) =
      P(tags("simple.f2")){(_: ST[Unit]) => s"foo $i".point[Future]}
        .flatMap(a => P(tags("simple.f2.1")){(_: ST[Unit]) => s"$a.1".point[Future]})
        .flatMap(a => P(tags("simple.f2.2")){(_: ST[Unit]) => s"$a.2".point[Future]})
    def f3(s: String) =
      P(tags("simple.f3")){(_: ST[Unit]) => s"$s finito".point[Future]}

    val res = P(tags("root")) {
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
    val f1 = P(tags("opt"))((_: ST[Unit]) => Option("foo").point[Future])
    val f2 = P(tags("opt"))((_: ST[Unit]) => Option(1).point[Future])
    val f3 = P(tags("opt"))((_: ST[Unit]) => (None: Option[Int]).point[Future])


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
    val f1 = P(tags("listT"))((_: ST[Unit]) => List("foo", "bar").point[Future])
    val f2 = P(tags("listT"))((_: ST[Unit]) => List(1, 2).point[Future])
    val f3 = P(tags("listT"))((_: ST[Unit]) => List[Int]().point[Future])

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

    val f1: P[String \/ String] =
      P(tags("f1"))(_ => \/-("foo").point[Future])
    val f2: P[String \/ Int] =
      P(tags("f2"))(_ => \/-(1).point[Future])
    val f3: P[String \/ String] =
      P(tags("f3"))(_ => -\/("Error").point[Future])

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

    val f1 = Option(1).point[P]

    optionT(f1).withFilter(_ => true).withFilter(_ => true).run.eval(nostate).futureValue should ===(Some(1))

  }

  it should "not stack overflow" in {
    def pre(l: List[Int], i: Int) = P(tags(s"stack_$i"))((_: ST[Unit]) => l.point[Future])

    val l = List.iterate(0, 100000){ i => i + 1 }

    val pf = l.foldLeft(
      pre(List(), 0)
    ){ case (p, i) =>
      p.flatMap(l => pre(i +: l, i))
    }

    pf.eval(nostate).futureValue should equal (l.reverse)
  }

*/
  it should "observe" in {
    import quiver.{ LNode, LEdge }

    implicit val intSG = new scalaz.Semigroup[Int] {
      def append(f1: Int, f2: => Int) = f1 + f2
    }

    def nostate = ST(Span.gen, env, Vector.empty, 0)

    type P[A] = Pre[Future, Int, A]
    val p0: P[Int] = P(tags("p0")).applyU((s: ST[Int]) => Future(0 -> 0))
    val p1: P[Int] = P(tags("p1")).applyU((s: ST[Int]) => Future(1 -> 1))
    val p2: P[Int] = P(tags("p2")).applyU((s: ST[Int]) => Future(2 -> 2))
    val p3: P[Int] = P(tags("p3")).applyU((s: ST[Int]) => Future(3 -> 3))
    val p4: P[Int] = P(tags("p4")).applyU((s: ST[Int]) => Future(4 -> 4))

    val p5 = 
      for {
      _ <- p0
      // _ <- (p1 |@| p2).tupled
      _ <- P(tags("sub"))(p4)
      _ <- p3
    } yield ()

    val (_, _, graph) = p5.observe(nostate).futureValue

    println(graph.viz)

    1 should ===(1)
  }

/*
  it should "real world wb.fr home" in {

    type ST = (Span, Call.Path[BaseTags]) => Log

    val taggingContext = new TaggingContext[BaseTags, ST[ST], Future]
    import taggingContext._
    import Precepte._
    import scalaz.std.option._

    trait Log {
      def debug(s: String): Unit
    }

    def Logged[A](tags: BaseTags)(f: Log => Future[A]): Precepte[A] =
      Precepte(tags) { (state: ST[ST]) =>
        f(state.value(state.span, state.path))
      }

    case class Board(pin: Option[Int])
    object BoardComp {
      def get() = Logged(tags("BoardComp.get")) { (logger: Log) =>
        logger.debug("BoardComp.get")
        Board(Option(1)).point[Future]
      }
    }

    case class Community(name: String)
    case class Card(name: String)

    object CardComp {
      def getPin(id: Int) = Logged(tags("BoardComp.getPin")) { (logger: Log) =>
        logger.debug("CardComp.getPin")
        Option(1 -> Card("card 1")).point[Future]
      }

      def countAll() = Logged(tags("CardComp.countAll")) { (logger: Log) =>
        logger.debug("CardComp.countAll")
        Set("Edito", "Video").point[Future]
      }

      def rank() = Logged(tags("CardComp.rank")) { (logger: Log) =>
        logger.debug("CardComp.rank")
        List(1 -> Card("foo"), 1 -> Card("bar")).point[Future]
      }

      def cardsInfos(cs: List[(Int, Card)], pin: Option[Int]) = Logged(tags("CardComp.cardsInfos")) { (logger: Log) =>
        logger.debug("CardComp.cardsInfos")
        List(
          Card("foo") -> List(Community("community 1"), Community("community 2")),
          Card("bar") -> List(Community("community 2"))).point[Future]
      }
    }

    import java.net.URL
    case class Highlight(title: String, cover: URL)
    object HighlightComp {
      def get() = Logged(tags("HighlightComp.get")) { (logger: Log) =>
        logger.debug("HighlightComp.get")
        Highlight("demo", new URL("http://nd04.jxs.cz/641/090/34f0421346_74727174_o2.png")).point[Future]
      }
    }

    val logs = scala.collection.mutable.ArrayBuffer[String]()

    case class Logger(span: Span, path: Call.Path[BaseTags]) extends Log {
      def debug(s: String): Unit = {
        logs += s"[DEBUG] ${span.value} -> /${path.mkString("/")} $s"
        ()
      }
    }

    val getPin =
      (for {
        b   <- trans(BoardComp.get().lift[Option])
        id  <- trans(Precepte(tags("point"))((_: ST[ST]) => b.pin.point[Future]))
        pin <- trans(CardComp.getPin(id))
      } yield pin).run


    val res = for {
      pin            <- getPin
      cs             <- CardComp.rank()
      cards          <- CardComp.cardsInfos(cs, pin.map(_._1))
      availableTypes <- CardComp.countAll()
      h              <- HighlightComp.get()
    } yield (pin, cs, cards, availableTypes, h)

    def logger(span: Span, path: Call.Path[BaseTags]): Log =
      Logger(span, path)

    val initialState = ST[ST](Span.gen, env, Vector.empty, logger _)
    res.eval(initialState).futureValue should ===(
      (Some((1, Card("card 1"))),
        List((1, Card("foo")), (1, Card("bar"))),
        List(
          (Card("foo"), List(
            Community("community 1"),
            Community("community 2"))),
          (Card("bar"),List(
            Community("community 2")))),
        Set("Edito", "Video"),
        Highlight("demo", new URL("http://nd04.jxs.cz/641/090/34f0421346_74727174_o2.png")))
    )

    for(l <- logs)
    println(l)
  }

  it should "implement flatMapK" in {

    def f1: Precepte[Int] =
      Precepte(tags("f1")) { (c: ST[Unit]) =>
        1.point[Future]
      }

    def f2: Precepte[Int] =
      Precepte(tags("f2")){ (c: ST[Unit]) =>
        Future { throw new RuntimeException("ooopps f2") }
      }

    def f3(i: Int): Precepte[String] =
      Precepte(tags("f3")){ (c: ST[Unit]) =>
        "foo".point[Future]
      }

    def f4(i: Int): Precepte[String] =
      Precepte(tags("f4")){ (c: ST[Unit]) =>
        Future { throw new RuntimeException("ooopps f4") }
      }

    (for {
      i <- f2
      // r <- f3(i)
    } yield i)
      .flatMapK{ fut =>
        Precepte(tags("f5")){ (c: ST[Unit]) => fut.recover { case _ => "recovered" } }
      }
      .eval(nostate).futureValue should ===("recovered")

    (for {
      i <- f1
      r <- f4(i)
    } yield r)
      .flatMapK{ fut =>
        Precepte(tags("f6")){ (c: ST[Unit]) => fut.recover { case _ => "recovered" } }
      }
      .eval(nostate).futureValue should ===("recovered")

  }

  it should "run flatMapK" in {

    def f1: Precepte[Int] =
      Precepte(tags("f1")) { (c: ST[Unit]) =>
        1.point[Future]
      }

    val (g, a) = f1.flatMapK(futI => Precepte(tags("f")){ _ => futI.map(i => (i+1)) }).run(nostate).futureValue
    a should equal (2)
  }
*/



/*


  it should "mapStepState" in {
    import scalaz.~>
    def f1 = Precepte(tags("simple.f1")){(_: ST[Unit]) => 1.point[Future]}
    def f2(i: Int) = Precepte(tags("simple.f2")){(_: ST[Unit]) => s"foo $i".point[Future]}

    val res = for {
      i <- f1
      r <- f2(i)
    } yield r

    def now() = System.nanoTime()

    val f = new (StepState ~> StepState) {
      def apply[A](fa: StepState[A]) = {
        fa.mapK { fsa =>
          val t = now()
          fsa.map { x =>
            val diff = now() - t
            println(s"Execution Time: $diff")
            println(s"State was: $x")
            x
          }
        }
      }
    }

    val (a, s) = res.mapStep(f).run(nostate).futureValue

  }

  it should "iso" in {
    def pre(l: List[Int], i: Int) = Precepte(tags(s"stack_$i"))((_: ST[Unit]) => l.point[Future])

    import scalaz.{ ~>, <~, NaturalTransformation }
    import scalaz.Isomorphism.<~>

    val to0 = new (Future ~> Future) {
      def apply[A](fa: Future[A]) = {
        val f = Future {
          println("before")
        }.flatMap(_ => fa)
        // .map{a => println(s"this is $a"); a}
        f.onComplete( _ => println("after") )
        f
      }
    }

    val from0 = NaturalTransformation.refl[Future]

    val iso0 = new (Future <~> Future) {
      def from = from0
      def to = to0
    }
    val tagiso = taggingContext.iso(iso0)

    val l = List.iterate(0, 20000){ i => i + 1 }

    val pf = l.foldLeft(
      pre(List(), 0)
    ){ case (p, i) =>
      p.flatMap(l => pre(i +: l, i))
    }

    // pf.eval(nostate).futureValue should equal (l.reverse)

    tagiso.iso.to(pf).eval(nostate).futureValue
  }


  it should "iso state" in {
    trait DB {
      def callDB(): Future[String] = Future { "DB.callDB" }
    }
    trait Logger {
      def log(str: String): Future[String] = Future { s"log $str" }
    }
    trait Service {
      def doit(): Future[String] = Future { "Service.doit" }
    }
    case class S(db: DB, logger: Logger, service: Service, ctx: Seq[String])
    case class S2(db: DB, logger: Logger, ctx: Seq[String])

    val tctx = new PCTX0[Future, S]

    def f1(): tctx.Precepte[String] =
      tctx.Precepte(tags("f1")).applyS { (s: ST[S]) =>
        s.unmanaged.value.service.doit().map { a =>
          s.unmanaged.copy(value = s.unmanaged.value.copy(ctx = s.unmanaged.value.ctx :+ a)) -> a
        }
      }

    val db = new DB {}
    val logger = new Logger {}
    val service = new Service {}

    val tctxiso = tctx.isoUnmanagedState(
      (s: PES0[S]) => s.copy(value = S2(s.value.db, s.value.logger, s.value.ctx)),
      (s: PES0[S2]) => s.copy(value = S(s.value.db, s.value.logger, service, s.value.ctx))
    )

    def f2(): tctxiso.tc.Precepte[String] =
      tctxiso.tc.Precepte(tags("f2")).applyS{ (s: ST[S2]) =>
        s.unmanaged.value.db.callDB().map { a =>
          s.unmanaged.copy(value = s.unmanaged.value.copy(ctx = s.unmanaged.value.ctx :+ a)) -> a
        }
      }

    val p = for {
      a <- f1
      b <- tctxiso.iso.from(f2())
    } yield (())

    val nostate = ST[S](Span.gen, env, Vector.empty, S(db, logger, service, Seq()))

    println("RES:"+p.run(nostate).futureValue)


    // val tagiso = taggingContext.iso(iso0)
  }
*/
}
