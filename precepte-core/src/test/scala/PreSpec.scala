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

  // val taggingContext = new PCTX0[Future, Unit]
  // import taggingContext._
  import Precepte._

  type P[A] = Precepte0[Future, Unit, A]

  object P {
    def apply[A](tags: BaseTags) = Precepte[BaseTags, PIS0, PES0[Unit], Future](tags)
  }

  val env = BaseEnv(Tags.Host("localhost"), Tags.Environment.Test, Tags.Version("1.0"))

  private def tags(n: String) = BaseTags(Tags.Callee(n), Tags.Category.Database)

  def nostate = PST0[Unit](Span.gen, env, Vector.empty, ())

  val ids = PIdStream((1 to 30).map(i => PId(i.toString)).toStream)

  import Tags.Callee

  "Precepte" should "run/eval simple" in {
    def f1 = P(tags("simple.f1")){(_: PST0[Unit]) => 1.point[Future]}
    def f2(i: Int) = P(tags("simple.f2")){(_: PST0[Unit]) => s"foo $i".point[Future]}

    val res = for {
      i <- f1
      r <- f2(i)
    } yield r

    val (s, a) = res.run(nostate).futureValue
    a should ===("foo 1")

    println(s"$s")
    s.unmanaged.env should ===(env)
    s.managed.path.size should ===(2)
    s.managed.path(0).tags should ===(tags("simple.f1"))
    s.managed.path(1).tags should ===(tags("simple.f2"))

    val a0 = res.eval(nostate).futureValue
    a0 should ===("foo 1")
  }



  it should "observe simple" in {
    def f1 =
      P(tags("simple.f1")){(_: PST0[Unit]) => 1.point[Future]}
        .flatMap(a => P(tags("simple.f1.1")){(_: PST0[Unit]) => (a+1).point[Future]})
        .flatMap(a => P(tags("simple.f1.2")){(_: PST0[Unit]) => (a+1).point[Future]})
        .flatMap(a => P(tags("simple.f1.3")){(_: PST0[Unit]) => (a+1).point[Future]})
    def f2(i: Int) =
      P(tags("simple.f2")){(_: PST0[Unit]) => s"foo $i".point[Future]}
        .flatMap(a => P(tags("simple.f2.1")){(_: PST0[Unit]) => s"$a.1".point[Future]})
        .flatMap(a => P(tags("simple.f2.2")){(_: PST0[Unit]) => s"$a.2".point[Future]})
    def f3(s: String) =
      P(tags("simple.f3")){(_: PST0[Unit]) => s"$s finito".point[Future]}

    val res = P(tags("root")) {
      for {
        i <- f1
        s <- f2(i)
        r <- f3(s)
      } yield r
    }

    val (s, a, graph) = res.observe(nostate).futureValue
    println("-- graph0 --")
    for {
      g <- graph
    } println(g.managed)


    // val g = tree.drawTree
    // println(g)
    // println("----")

    // val sf1 = tree.subForest(0)
    // val Node0(_, t1) = sf1.rootLabel
    // t1 should ===(tags("simple.f1"))
    // val Node0(_, t11) = sf1.subForest(0).rootLabel
    // t11 should ===(tags("simple.f1.1"))
    // val Node0(_, t12) = sf1.subForest(1).rootLabel
    // t12 should ===(tags("simple.f1.2"))
    // val Node0(_, t13) = sf1.subForest(2).rootLabel
    // t13 should ===(tags("simple.f1.3"))

    // val sf2 = tree.subForest(1)
    // val Node0(_, t2) = sf2.rootLabel
    // t2 should ===(tags("simple.f2"))
    // val Node0(_, t21) = sf2.subForest(0).rootLabel
    // t21 should ===(tags("simple.f2.1"))
    // val Node0(_, t22) = sf2.subForest(1).rootLabel
    // t22 should ===(tags("simple.f2.2"))

    // val sf3 = tree.subForest(2)
    // val Node0(_, t3) = sf3.rootLabel
    // t3 should ===(tags("simple.f3"))

  }


  it should "OptT" in {
    val f1 = P(tags("opt"))((_: PST0[Unit]) => Option("foo").point[Future])
    val f2 = P(tags("opt"))((_: PST0[Unit]) => Option(1).point[Future])
    val f3 = P(tags("opt"))((_: PST0[Unit]) => (None: Option[Int]).point[Future])


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
    val f1 = P(tags("listT"))((_: PST0[Unit]) => List("foo", "bar").point[Future])
    val f2 = P(tags("listT"))((_: PST0[Unit]) => List(1, 2).point[Future])
    val f3 = P(tags("listT"))((_: PST0[Unit]) => List[Int]().point[Future])

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

    val (_, rr0, graph0) = res3.run.observe(nostate).futureValue
    rr0 should ===(error)
    println("-- graph1 --")
    println(graph0)
    // val tree = graph0.toTree
    // println(tree.drawTree)
    println("----")

    // val sf = tree.subForest(0)
    // sf.subForest should be(empty)
    // val Node0(_, t) = sf.rootLabel
    // t should ===(tags("f3"))

    // val (rr1, _, _, graph1) = res3.run.observe(nostate).futureValue
    // rr1 should ===(error)
    // println("-- graph1 --")
    // p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph1)
    // println("----")

  }

/*

  it should "trivial" in {

    def f1 = Precepte(tags("trivial.f1")){ (_: PST0[Unit]) => 1.point[Future] }
    def f2(i: Int) = Precepte(tags("trivial.f2")){ (_: PST0[Unit]) => s"foo $i".point[Future] }
    def f3(i: Int) = Precepte(tags("trivial.f3")){ (_: PST0[Unit]) => (i + 1).point[Future] }

    val (result0, _, _, graph0) = f1.observe(nostate, ids).futureValue
    result0 should ===(1)
    graph0.children.size should ===(1)
    graph0.children(0).id should ===(PId("1"))

    println("-- graph0 --")
    p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph0)
    println("----")

    val (_, _, _, graphm) = Precepte(tags("graphm0"))(Precepte(tags("graphm"))(f1)).observe(nostate).futureValue
    println("-- graphm --")
    p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graphm)
    println("----")

    val res =
      for {
        i <- f1
        r <- f2(i)
      } yield r

    val (result11, _, _, graph11) = res.observe0(nostate, ids).futureValue
    println("-- graph11 --")
    val tree11 = graph11.toTree
    println(tree11.drawTree)
    println("----")

    result11 should ===("foo 1")

    tree11.subForest should have size(2)
    val sf110 = tree11.subForest(0)
    sf110.subForest should be(empty)
    val Node0(_, t110) = sf110.rootLabel
    t110 should ===(tags("trivial.f1"))

    val sf111 = tree11.subForest(1)
    sf111.subForest should be(empty)
    val Node0(_, t111) = sf111.rootLabel
    t111 should ===(tags("trivial.f2"))

    // val (result12, _, _, graph12) = res.observe(nostate, ids).futureValue
    // graph12.children.size should ===(2)
    // graph12.children(0).id should ===(PId("1"))
    // graph12.children(1).id should ===(PId("2"))

    // result12 should ===("foo 1")

    // println("-- graph12 --")
    // p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph12)
    // println("----")

    val (result21, _, _, graph21) = Precepte(tags("trivial.anon"))(res).observe0(nostate).futureValue

    println("-- graph21 --")
    val tree21 = graph21.toTree
    println(tree21.drawTree)
    println("----")


    val sf211 = tree21.subForest(0)
    sf211.subForest should be(empty)
    val Node0(_, t211) = sf211.rootLabel
    t211 should ===(tags("trivial.anon"))

    val sf212 = tree21.subForest(1)
    sf212.subForest should be(empty)
    val Node0(_, t212) = sf212.rootLabel
    t212 should ===(tags("trivial.f1"))

    val sf213 = tree21.subForest(2)
    sf213.subForest should be(empty)
    val Node0(_, t213) = sf213.rootLabel
    t213 should ===(tags("trivial.f2"))


    // val (result22, _, _, graph22) = Precepte(tags("trivial.anon"))(res).observe(nostate).futureValue

    // println("-- graph22 --")
    // p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph22)
    // println("----")

    // val res2 =
    //   for {
    //     i <- Precepte(tags("trivial.anon2"))(f1)
    //     r <- f2(i)
    //   } yield r

    // val (result2, _, _, graph2) = res2.observe(nostate, ids).futureValue
    // println("-- graph2 --")
    // p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph2)
    // println("----")
  }


  it should "pass context" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[PST0[Unit]]()

    def push(state: PST0[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")){ (c: PST0[Unit]) =>
      push(c)
      1.point[Future]
    }

    val (res0, _, _, graph0) = f1.observe0(nostate).futureValue
    res0 should ===(1)
    ctxs.length should ===(1)
    // ctxs.toList should ===(toStates(graph).toList)

    println("-- graph0 --")
    val tree0 = graph0.toTree
    println(tree0.drawTree)
    println("----")

    val sf0 = tree0.subForest(0)
    sf0.subForest should be(empty)
    val Node0(_, t0) = sf0.rootLabel
    t0 should ===(tags("f1"))


    // ctxs.clear()

    // val (res, _, _, graph) = f1.observe(nostate).futureValue
    // res should ===(1)
    // ctxs.length should ===(1)

    // println("-- graph --")
    // p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph)
    // println("----")

  }

  it should "preserve context on map" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[PST0[Unit]]()

    def push(state: PST0[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")){ (c: PST0[Unit]) =>
      push(c)
      1.point[Future]
    }.map(identity).map(identity).map(identity).map(identity)

    val (res0, _, _, graph0) = f1.observe0(nostate).futureValue
    res0 should ===(1)
    ctxs.length should ===(1)

    println("-- graph0 --")
    val tree0 = graph0.toTree
    println(tree0.drawTree)
    println("----")


    val sf0 = tree0.subForest(0)
    sf0.subForest should be(empty)
    val Node0(_, t0) = sf0.rootLabel
    t0 should ===(tags("f1"))

    // ctxs.clear()

    // val (res, _, _, graph) = f1.observe(nostate).futureValue
    // res should ===(1)

    ctxs.length should ===(1)
    ctxs.head.path.length should ===(1)
    // ctxs.toList should ===(toStates(graph).toList)

    // println("-- graph --")
    // p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph)
    // println("----")

  }

  it should "preserve context on flatMap" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[PST0[Unit]]()

    def push(state: PST0[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")){ (c: PST0[Unit]) =>
      push(c)
      1.point[Future]
    }

    def f2(i: Int) = Precepte(tags("f2")){ (c: PST0[Unit]) =>
      push(c)
      s"foo $i".point[Future]
    }

    def f3(s: String) = Precepte(tags("f3")){ (c: PST0[Unit]) =>
      push(c)
      s"f3 $s".point[Future]
    }

    val f = Precepte(tags("anon0"))(f1
      .flatMap(i => f2(i))
      .flatMap(s => f3(s)))

    val (res, _, _, graph0) = f.observe0(nostate).futureValue
    res should ===("f3 foo 1")

    ctxs.length should ===(3)
    // ctxs.toList should ===(toStates(graph).toList.drop(1))

    println("-- graph00 --")
    val tree0 = graph0.toTree
    println(tree0.drawTree)
    println("----")

    val sf0 = tree0.subForest(0)
    sf0.subForest should have size(3)
    val Node0(_, t0) = sf0.rootLabel
    t0 should ===(tags("anon0"))

    val sf00 = sf0.subForest(0)
    sf00.subForest should be(empty)
    val Node0(_, t00) = sf00.rootLabel
    t00 should ===(tags("f1"))

    val sf01 = sf0.subForest(1)
    sf01.subForest should be(empty)
    val Node0(_, t01) = sf01.rootLabel
    t01 should ===(tags("f2"))

    val sf02 = sf0.subForest(2)
    sf02.subForest should be(empty)
    val Node0(_, t02) = sf02.rootLabel
    t02 should ===(tags("f3"))

  }

  it should "stack contexts" in {
    def f1 = Precepte(tags("f1")){ (c: PST0[Unit]) =>
      1.point[Future]
    }

    val stacked = Precepte(tags("stacked"))(f1)
    val (r, _, _, graph) = stacked.observe0(nostate).futureValue
    r should ===(1)

    println("-- graph --")
    val tree0 = graph.toTree
    println(tree0.drawTree)
    println("----")

    val sf0 = tree0.subForest(0)
    sf0.subForest should have size(1)
    val Node0(_, t0) = sf0.rootLabel
    t0 should ===(tags("stacked"))
    sf0.subForest should have size(1)
    val t00 = sf0.subForest(0)
    t00.subForest should be(empty)
    val Node0(_, t000) = t00.rootLabel
    t000 should ===(tags("f1"))

    // graph.children should have length 1
    // graph.children.head.children should have length 1
  }

  it should "provide context to C" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[PST0[Unit]]()

    def push(state: PST0[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")) { (c: PST0[Unit]) =>
      push(c)
      1.point[Future]
    }

    def f2(i: Int) = Precepte(tags("f2")){ (c: PST0[Unit]) =>
      push(c)
      s"foo $i".point[Future]
    }

    // val (res, _, _, graph) = f1.observe0(nostate).futureValue
    // res should ===(1)
    // ctxs.length should ===(1)
    // ctxs.head.path.length should ===(1)

    // ctxs.clear()

    // val res2 = f1.map(identity)
    // res2.eval(nostate)

    // ctxs should have length(1)
    // forAll(ctxs.map(_.path.length == 1)){_  should ===(true) }


    // ctxs.clear()

    val r = for {
      i <- f1
      r <- f2(i)
    } yield r

    r.eval(nostate).futureValue should ===("foo 1")

    // val (r0, _, _, graph0) = r.observe0(nostate).futureValue

    // println("-- graph0 --")
    // val tree0 = graph0.toTree
    // println(tree0.drawTree)
    // println("----")


    ctxs should have length(2)
    ctxs.map(_.span).toSet.size should ===(1) // span is unique
    println("===> CTX:"+ctxs)
    forAll(ctxs.map(_.path.length == 1)){ _ should ===(true) }

    // ctxs.clear()

    // val res3 = Precepte(tags("res3"))(f1)
    // res3.eval(nostate).futureValue should ===(1)

    // ctxs should have length(1)
    // ctxs.map(_.span).toSet.size should ===(1) // span is unique
    // forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }

    // ctxs.clear()

    // val res4 = Precepte(tags("res4")) {
    //   for {
    //     i <- f1
    //     r <- f2(i)
    //   } yield r
    // }

    // res4.eval(nostate).futureValue should ===("foo 1")

    // ctxs should have length(2)
    // ctxs.map(_.span).toSet.size should ===(1) // span is unique
    // forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }
  }
*/
/*
  it should "not stack context on trans" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[PST0[Unit]]()

    def push(state: PST0[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")) { (c: PST0[Unit]) =>
      push(c)
      Option(1).point[Future]
    }

    def f2(i: Int) = Precepte(tags("f1")){ (c: PST0[Unit]) =>
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
    ctxs.map(_.span).toSet.size should ===(1) // span is unique
    forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }

  }

  it should "real world wb.fr home" in {

    type ST = (Span, Call.Path[BaseTags]) => Log

    val taggingContext = new TaggingContext[BaseTags, PST0[ST], Future]
    import taggingContext._
    import Precepte._
    import scalaz.std.option._

    trait Log {
      def debug(s: String): Unit
    }

    def Logged[A](tags: BaseTags)(f: Log => Future[A]): Precepte[A] =
      Precepte(tags) { (state: PST0[ST]) =>
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
        id  <- trans(Precepte(tags("point"))((_: PST0[ST]) => b.pin.point[Future]))
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

    val initialState = PST0[ST](Span.gen, env, Vector.empty, logger _)
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
      Precepte(tags("f1")) { (c: PST0[Unit]) =>
        1.point[Future]
      }

    def f2: Precepte[Int] =
      Precepte(tags("f2")){ (c: PST0[Unit]) =>
        Future { throw new RuntimeException("ooopps f2") }
      }

    def f3(i: Int): Precepte[String] =
      Precepte(tags("f3")){ (c: PST0[Unit]) =>
        "foo".point[Future]
      }

    def f4(i: Int): Precepte[String] =
      Precepte(tags("f4")){ (c: PST0[Unit]) =>
        Future { throw new RuntimeException("ooopps f4") }
      }

    (for {
      i <- f2
      // r <- f3(i)
    } yield i)
      .flatMapK{ fut =>
        Precepte(tags("f5")){ (c: PST0[Unit]) => fut.recover { case _ => "recovered" } }
      }
      .eval(nostate).futureValue should ===("recovered")

    (for {
      i <- f1
      r <- f4(i)
    } yield r)
      .flatMapK{ fut =>
        Precepte(tags("f6")){ (c: PST0[Unit]) => fut.recover { case _ => "recovered" } }
      }
      .eval(nostate).futureValue should ===("recovered")

  }

  it should "run flatMapK" in {

    def f1: Precepte[Int] =
      Precepte(tags("f1")) { (c: PST0[Unit]) =>
        1.point[Future]
      }

    val (g, a) = f1.flatMapK(futI => Precepte(tags("f")){ _ => futI.map(i => (i+1)) }).run(nostate).futureValue
    a should equal (2)
  }
*/
  it should "not break type inference" in {
    import scalaz.syntax.monadPlus._
    import scalaz.syntax.Ops
    import scalaz.OptionT._
    import scalaz._
    import scala.language.implicitConversions
    // import shapeless._

    val f1 = Option(1).point[P]

    // implicitly[Monad[P]]
    // implicitly[Applicative[P]]
    // implicitly[MonadPlus[P]]

    trait Dummy1[F[_]]
    object Dummy1 {

      implicit def mkDummy1[F[_]]: Dummy1[F] = new Dummy1[F] {}
      // implicit def f(
      //   implicit nosi: NoSI2712[Dummy1]
      // ) = nosi.mkTC

    }

    // final class MonadPlusOps[F[_],A](val self: F[A])(implicit val F: MonadPlus[F]) extends Ops[F[A]] {
    //   ////
    //   import Leibniz.===

    //   def filter(f: A => Boolean) =
    //     F.filter(self)(f)

    //   def withFilter(f: A => Boolean) =
    //     filter(f)

    //   final def uniteU[T](implicit T: Unapply[Foldable, A]): F[T.A] =
    //     F.uniteU(self)(T)

    //   def unite[T[_], B](implicit ev: A === T[B], T: Foldable[T]): F[B] = {
    //     val ftb: F[T[B]] = ev.subst(self)
    //     F.unite[T, B](ftb)
    //   }

    //   final def separate[G[_, _], B, C](implicit ev: A === G[B, C], G: Bifoldable[G]): (F[B], F[C]) =
    //     F.separate(ev.subst(self))

    //   ////
    // }

    // implicitly[Dummy1[P]]
    // implicitly[Dummy1[({ type λ[α] = Pre[Tags, PIS0, PES0[Unit], Future, α] })#λ]]
    // implicitly[Monad[({ type λ[α] = Pre[Tags, PIS0, PES0[Unit], Future, α] })#λ]]

    // Split1[({ type λ[t] = List[(t, t)] })#λ, Dummy1, Dummy1]

    // val n = new NoSI2712H[Monad] {}
    // import n._
    // implicit def f[TC[_[_]]](
    //   implicit nosi: NoSI2712[TC]
    // ) = nosi.mkTC

    // val a0 = f[Dummy1]
    // implicitly[Dummy1[({ type λ[α] = Pre0[Future, Unit, α] })#λ]]

    // implicitly[Una[OptionT[P, Int], OptionT]]

    // implicit def koko[L[_], A](l: L[A])(implicit split1: Split1[L, Dummy1, Dummy1]): split1.O[split1.I[A]] = {
    //   split1.unpack(l)
    // }

    // implicit def koko[TPA, TC[_[_], _], F[_[_]]](l: TPA)(implicit una: Una[TPA, TC, F]): TC[una.P, una.A] = una.unpack(l)
    // koko(List((5, 5)))

    // type T = OptionT[P, Int]
    // implicitly[Unapply[MonadPlus, OptionT[({ type λ[α] = Pre0[Future, Unit, α] })#λ, Int]]]

    // import scalaz.Leibniz.===
    // val leibniz: OptionT[({ type λ[α] = Pre0[Future, Unit, α] })#λ, Int] === OptionT[({ type λ[α] = Pre0[Future, Unit, α] })#λ, Int] = scalaz.Leibniz.refl

    // import scalaz.syntax.ext.MonadPlusOpsExt._

    // PreHackSI2712.materialize[
    //   OptionT[({ type λ[α] = Pre0[Future, Unit, α] })#λ, Int],
    //   OptionT,
    //   MonadPlus,
    //   Future,
    //   BaseTags,
    //   PIS0,
    //   PES0[Unit],
    //   Int
    // ]

    val a = optionT(f1).withFilter(_ => true).withFilter(_ => true).run.eval(nostate).futureValue should ===(Some(1))


    // implicit def f[TCA](tca: TCA)(implicit nosi: NoSI2712T[TCA, OptionT, MonadPlus]) =
    //   new MonadPlusOps[nosi.T, nosi.A](nosi.leibniz(tca))(nosi.MTC)
      /*new Unapply[M0, TCA] {
        type M[x] = nosi.T[x]
        type A = nosi.A
        def TC = nosi.MTC
        def leibniz = leibniz
      }*/
    // type X[T] = OptionT[P, T]
    // implicitly[MonadPlus[X]]
    // type P[x] = ({ type λ[α] = Pre0[Future, Unit, α] })#λ[x]
    // type T[x] = OptionT[P, x]
    // val una = implicitly[Una[OptionT[({ type λ[α] = Pre0[Future, Unit, α] })#λ, Int], OptionT]]

    // implicitly[MonadPlus[({ type λ[α] = OptionT[P, α] })#λ]]
    // implicitly[MonadPlus[({ type λ[α] = OptionT[({ type λ[α] = Pre0[Future, Unit, α] })#λ, α] })#λ]]
    // shapeless.lazily[scalaz.MonadPlus[T]]

    // val una2 = implicitly[Una2[OptionT[({ type λ[α] = Pre0[Future, Unit, α] })#λ, Int], OptionT, Future, Int]]

    // implicitly[NoSI2712T2[OptionT[({ type λ[α] = Pre0[Future, Unit, α] })#λ, Int], OptionT, MonadPlus, Future, Int]]

    // implicit val s = SingletonOf.mkSingletonOf[
    //   OptionT,
    //   Una[
    //     OptionT[({ type λ[α] = Pre0[Future, Unit, α] })#λ, Int],
    //     OptionT
    //   ]
    // ]


  }



/*
  it should "eval flatMapK failure" in {

    def f1: Precepte[Int] =
      Precepte(tags("f1")) { (c: PST0[Unit]) =>
        Future { throw new RuntimeException("ooopps f1") }
      }

    val a = f1
      .flatMapK(futI => Precepte(tags("f")){ _ => Future("recover") })
      .flatMap { a => println(s"A:$a"); 1.point[Precepte] }
      .eval(nostate).futureValue
    a should equal (1)
  }



  it should "not stack overflow" in {
    def pre(l: List[Int], i: Int) = P(tags(s"stack_$i"))((_: PST0[Unit]) => l.point[Future])

    val l = List.iterate(0, 100000){ i => i + 1 }

    val pf = l.foldLeft(
      pre(List(), 0)
    ){ case (p, i) =>
      p.flatMap(l => pre(i +: l, i))
    }

    pf.eval(nostate).futureValue should equal (l.reverse)
  }


  it should "mapStepState" in {
    import scalaz.~>
    def f1 = Precepte(tags("simple.f1")){(_: PST0[Unit]) => 1.point[Future]}
    def f2(i: Int) = Precepte(tags("simple.f2")){(_: PST0[Unit]) => s"foo $i".point[Future]}

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
    def pre(l: List[Int], i: Int) = Precepte(tags(s"stack_$i"))((_: PST0[Unit]) => l.point[Future])

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
      tctx.Precepte(tags("f1")).applyS { (s: PST0[S]) =>
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
      tctxiso.tc.Precepte(tags("f2")).applyS{ (s: PST0[S2]) =>
        s.unmanaged.value.db.callDB().map { a =>
          s.unmanaged.copy(value = s.unmanaged.value.copy(ctx = s.unmanaged.value.ctx :+ a)) -> a
        }
      }

    val p = for {
      a <- f1
      b <- tctxiso.iso.from(f2())
    } yield (())

    val nostate = PST0[S](Span.gen, env, Vector.empty, S(db, logger, service, Seq()))

    println("RES:"+p.run(nostate).futureValue)


    // val tagiso = taggingContext.iso(iso0)
  }
*/
}
