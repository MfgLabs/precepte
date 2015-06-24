package com.mfglabs
package precepte

import org.scalatest._
import Matchers._
// import Inspectors._

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

  val taggingContext = new PCTX0[Future, Unit]
  import taggingContext._
  // import Precepte._

  val env = BaseEnv(Tags.Host("localhost"), Tags.Environment.Test, Tags.Version("1.0"))

  private def tags(n: String) = BaseTags(Tags.Callee(n), Tags.Category.Database)

  def p[C, S <: PST0[C], G <: Graph[BaseTags, S, G]](g: G, before: String = ""): Unit = {
    val txt = g match {
      case Root(span, _) =>
        s"Root[$span]"
      case GraphNode(id, _, tags, _) =>
        s"GraphNode[$id]: $tags"
    }

    println(before + txt)

    for (c <- g.children) {
      c match {
        case node@GraphNode(_, _, _, _) =>
          p[C, S, GraphNode[BaseTags, S]](node, before + "  ")
      }
    }
  }

  def toStates[C](g: Root[BaseTags, PST0[C]]): Seq[PST0[C]] = {
    def go(g: GraphNode[BaseTags, PST0[C]], span: Span, path: Call.Path[BaseTags], states: Seq[PST0[C]]): Seq[PST0[C]] = {
      val GraphNode(id, value, tags, cs) = g
      val p = path :+ Call(id, tags)
      val st = PST0[C](span, env, p, value.value)
      val cst = cs.map{ c =>
        go(c, span, p, Seq.empty)
      }.flatten
      (states :+ st) ++ cst
    }

    g.children.map { c =>
      go(c, g.span, Vector.empty, Seq.empty)
    }.flatten
  }


  def nostate = PST0[Unit](Span.gen, env, Vector.empty, ())
  import Tags.Callee
/*
  "Precepte" should "trivial" in {

    def f1 = Precepte(tags("trivial.f1")){ (_: PST0[Unit]) => 1.point[Future] }
    def f2(i: Int) = Precepte(tags("trivial.f2")){ (_: PST0[Unit]) => s"foo $i".point[Future] }
    def f3(i: Int) = Precepte(tags("trivial.f3")){ (_: PST0[Unit]) => (i + 1).point[Future] }

    val (graph0, result0) = f1.run(nostate).futureValue
    result0 should ===(1)

    println("-- graph0 --")
    p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph0)
    println("----")

    val (graphm, _) = Precepte(tags("graphm0"))(Precepte(tags("graphm"))(f1)).run(nostate).futureValue
    println("-- graphm --")
    p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graphm)
    println("----")

    val res =
      for {
        i <- f1
        r <- f2(i)
      } yield r

    val (graph, result) = res.run(nostate).futureValue
    result should ===("foo 1")

    println("-- graph --")
    p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph)
    println("----")

    val (graph1, result1) = Precepte(tags("trivial.anon"))(res).run(nostate).futureValue
    println("-- graph1 --")
    p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph1)
    println("----")

    val res2 =
      for {
        i <- Precepte(tags("trivial.anon2"))(f1)
        r <- f2(i)
      } yield r

    val (graph2, result2) = res2.run(nostate, (1 to 30).map(i => CId(i.toString)).toStream).futureValue
    println("-- graph2 --")
    p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph2)
    println("----")
  }

  it should "simple" in {
    def f1 = Precepte(tags("simple.f1")){(_: PST0[Unit]) => 1.point[Future]}
    def f2(i: Int) = Precepte(tags("simple.f2")){(_: PST0[Unit]) => s"foo $i".point[Future]}

    val res = for {
      i <- f1
      r <- f2(i)
    } yield r

    res.eval(nostate).futureValue should ===("foo 1")
  }

  it should "optT" in {
    val f1 = Precepte(tags("opt"))((_: PST0[Unit]) => Option("foo").point[Future])
    val f2 = Precepte(tags("opt"))((_: PST0[Unit]) => Option(1).point[Future])
    val f3 = Precepte(tags("opt"))((_: PST0[Unit]) => (None: Option[Int]).point[Future])


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


  it should "listT" in {
    val f1 = Precepte(tags("listT"))((_: PST0[Unit]) => List("foo", "bar").point[Future])
    val f2 = Precepte(tags("listT"))((_: PST0[Unit]) => List(1, 2).point[Future])
    val f3 = Precepte(tags("listT"))((_: PST0[Unit]) => List[Int]().point[Future])

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

    val f1: Precepte[String \/ String] =
      Precepte(tags("f1"))(_ => \/-("foo").point[Future])
    val f2: Precepte[String \/ Int] =
      Precepte(tags("f2"))(_ => \/-(1).point[Future])
    val f3: Precepte[String \/ String] =
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

    val (graph, rr) = res3.run.run(nostate).futureValue
    rr should ===(error)
    p[Unit, PST0[Unit], Root[BaseTags, PST0[Unit]]](graph)
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

    val (graph, res) = f1.run(nostate).futureValue
    res should ===(1)
    ctxs.length should ===(1)

    ctxs.toList should ===(toStates(graph).toList)
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

    val (graph, res) = f1.run(nostate).futureValue
    res should ===(1)

    ctxs.length should ===(1)
    ctxs.head.path.length should ===(1)

    ctxs.toList should ===(toStates(graph).toList)
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

    val (graph, res) = f.run(nostate).futureValue
    res should ===("f3 foo 1")

    ctxs.length should ===(3)
    ctxs.toList should ===(toStates(graph).toList.drop(1))
  }

  it should "stack contexts" in {
    def f1 = Precepte(tags("f1")){ (c: PST0[Unit]) =>
      1.point[Future]
    }

    val stacked = Precepte(tags("stacked"))(f1)
    val (graph, r) = stacked.run(nostate).futureValue
    r should ===(1)

    graph.children should have length 1
    graph.children.head.children should have length 1
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

    val (graph, res) = f1.run(nostate).futureValue
    res should ===(1)
    ctxs.length should ===(1)
    ctxs.head.path.length should ===(1)


    ctxs.clear()

    val res2 = f1.map(identity)
    res2.eval(nostate)

    ctxs should have length(1)
    forAll(ctxs.map(_.path.length == 1)){_  should ===(true) }


    ctxs.clear()

    val r = for {
      i <- f1
      r <- f2(i)
    } yield r

    r.eval(nostate).futureValue should ===("foo 1")

    ctxs should have length(2)
    ctxs.map(_.span).toSet.size should ===(1) // span is unique
    forAll(ctxs.map(_.path.length == 1)){ _ should ===(true) }

    ctxs.clear()

    val res3 = Precepte(tags("res3"))(f1)
    res3.eval(nostate).futureValue should ===(1)

    ctxs should have length(1)
    ctxs.map(_.span).toSet.size should ===(1) // span is unique
    forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }

    ctxs.clear()

    val res4 = Precepte(tags("res4")) {
      for {
        i <- f1
        r <- f2(i)
      } yield r
    }

    res4.eval(nostate).futureValue should ===("foo 1")

    ctxs should have length(2)
    ctxs.map(_.span).toSet.size should ===(1) // span is unique
    forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }
  }

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

  it should "implement mapK" in {

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
      r <- f3(i)
    } yield r)
      .flatMapK(_.map(_.point[Precepte]).recover { case _ => "recovered".point[Precepte] })
      .eval(nostate).futureValue should ===("recovered")

    (for {
      i <- f1
      r <- f4(i)
    } yield r)
      .flatMapK(_.map(_.point[Precepte]).recover { case _ => "recovered".point[Precepte] })
      .eval(nostate).futureValue should ===("recovered")

  }

  it should "run flatMapK" in {

    def f1: Precepte[Int] =
      Precepte(tags("f1")) { (c: PST0[Unit]) =>
        1.point[Future]
      }

    val (g, a) = f1.flatMapK(futI => futI.map(i => (i+1).point[Precepte])).run(nostate).futureValue
    a should equal (2)
  }

  it should "not break type inference" in {
    import scalaz.syntax.monadPlus._
    import scalaz.OptionT._
    val f1 = Option(1).point[Precepte]
    optionT(f1).withFilter(_ => true).withFilter(_ => true).run.eval(nostate).futureValue should ===(Some(1))
  }


*/

  it should "not stack overflow" in {
    def pre(i: Int) = Precepte(tags(s"stack_$i"))((_: PST0[Unit]) => List(i).point[Future])

    val f1 = Precepte(tags("stacko"))((_: PST0[Unit]) => List("foo", "bar").point[Future])
    val pf = List.fill(10000)(5).foldLeft(
      pre(0)
    ){ case (p, i) =>
      p.flatMap(_ => pre(i))
    }

    pf.eval(nostate).futureValue should equal (List(5))
  }
}
