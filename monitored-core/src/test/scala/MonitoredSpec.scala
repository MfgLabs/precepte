package com.mfglab.monitoring

import org.scalatest._
import Matchers._
import Inspectors._
import Inside._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span}

class MonitoredSpec extends FlatSpec with ScalaFutures {

  import Monitored._
  trait Log {
    def debug(s: String): Unit
  }

  implicit val defaultPatience =
    PatienceConfig(timeout =  Span(300, Seconds), interval = Span(5, Millis))

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import scalaz.std.scalaFuture._
  import scalaz.std.option._
  import scalaz.std.list._
  import scalaz.syntax.monad._
  import scalaz.EitherT

  def p[C, G <: Call.Graph[C, G]](g: G, before: String = ""): Unit = {
    val txt = g match {
      case Call.Root(span, _) =>
        s"Root[$span]"
      case Call.GraphNode(id, _, tags, _) =>
        s"GraphNode[$id]: $tags"
    }

    println(before + txt)

    for (c <- g.children) {
      c match {
        case node@Call.GraphNode(_, _, _, _) =>
          p[C, Call.GraphNode[C]](node, before + "  ")
      }
    }
  }

  def toStates[C](g: Call.Root[C]): Seq[Call.State[C]] = {
    def go(g: Call.GraphNode[C], span: Call.Span, path: Call.Path, states: Seq[Call.State[C]]): Seq[Call.State[C]] = {
      val Call.GraphNode(id, value, tags, cs) = g
      val p = path :+ Call(id, tags)
      val st = Call.State[C](span, p, value)
      val cst = cs.map{ c =>
        go(c, span, p, Seq.empty)
      }.flatten
      (states :+ st) ++ cst
    }

    g.children.map { c =>
      go(c, g.span, Vector.empty, Seq.empty)
    }.flatten
  }


  def nostate = Call.State(Call.Span.gen, Vector.empty, ())
  import Call.Tags
  import Tags.Callee

  "Monitored" should "trivial" in {

    def f1 = Monitored(Tags(Callee("trivial.f1"))).apply0{ (_: Call.State[Unit]) => 1 }
    def f2(i: Int) = Monitored(Tags(Callee("trivial.f2"))).apply0{(_: Call.State[Unit]) => s"foo $i"}
    def f3(i: Int) = Monitored(Tags(Callee("trivial.f3"))).apply0{(_: Call.State[Unit]) => i + 1}

    val (graph0, result0) = f1.run(nostate)
    result0 should ===(1)

    println("-- graph0 --")
    p[Unit, Call.Root[Unit]](graph0)
    println("----")

    val (graphm, _) = Monitored(Tags.empty)(Monitored(Tags.empty)(f1)).run(nostate)
    println("-- graphm --")
    p[Unit, Call.Root[Unit]](graphm)
    println("----")

    val res =
      for {
        i <- f1
        r <- f2(i)
      } yield r

    val (graph, result) = res.run(nostate)
    result should ===("foo 1")

    println("-- graph --")
    p[Unit, Call.Root[Unit]](graph)
    println("----")

    val (graph1, result1) = Monitored(Tags(Callee("trivial.anon")))(res).run(nostate)
    println("-- graph1 --")
    p[Unit, Call.Root[Unit]](graph1)
    println("----")

    val res2 =
      for {
        i <- Monitored(Tags(Callee("trivial.anon2")))(f1)
        r <- f2(i)
      } yield r

    val (graph2, result2) = res2.run(nostate, (1 to 30).map(i => Call.Id(i.toString)).toStream)
    println("-- graph2 --")
    p[Unit, Call.Root[Unit]](graph2)
    println("----")
  }

  it should "simple" in {
    def f1 = Monitored(Tags(Callee("simple.f1"))){(_: Call.State[Unit]) => 1.point[Future]}
    def f2(i: Int) = Monitored(Tags(Callee("simple.f2"))){(_: Call.State[Unit]) => s"foo $i".point[Future]}

    val res = for {
      i <- f1
      r <- f2(i)
    } yield r

    res.eval(nostate).futureValue should ===("foo 1")
  }

  it should "optT" in {
    val f1 = Monitored(Call.Tags.empty)((_: Call.State[Unit]) => Option("foo").point[Future])
    val f2 = Monitored(Call.Tags.empty)((_: Call.State[Unit]) => Option(1).point[Future])
    val f3 = Monitored(Call.Tags.empty)((_: Call.State[Unit]) => (None: Option[Int]).point[Future])


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
    val f1 = Monitored(Call.Tags.empty)((_: Call.State[Unit]) => List("foo", "bar").point[Future])
    val f2 = Monitored(Call.Tags.empty)((_: Call.State[Unit]) => List(1, 2).point[Future])
    val f3 = Monitored(Call.Tags.empty)((_: Call.State[Unit]) => List[Int]().point[Future])

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

    val f1: Monitored[Unit, Future, String \/ String] =
      Monitored(Call.Tags.empty)(_ => \/-("foo").point[Future])
    val f2: Monitored[Unit, Future, String \/ Int] =
      Monitored(Call.Tags.empty)(_ => \/-(1).point[Future])
    val f3: Monitored[Unit, Future, String \/ String] =
      Monitored(Call.Tags.empty)(_ => -\/("Error").point[Future])

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
    p[Unit, Call.Root[Unit]](graph)
  }

  case class Board(pin: Option[Int])
  object BoardComp {
    def get() = Monitored(Call.Tags.empty) { (c: Call.State[Log]) =>
      val logger = c.value
      logger.debug("BoardComp.get")
      Board(Option(1)).point[Future]
    }
  }

  case class Community(name: String)
  case class Card(name: String)

  object CardComp {
    def getPin(id: Int) = Monitored(Call.Tags.empty) { (c: Call.State[Log]) =>
      val logger = c.value
      logger.debug("CardComp.getPin")
      Option(1 -> Card("card 1")).point[Future]
    }

    def countAll() = Monitored(Call.Tags.empty) { (c: Call.State[Log]) =>
      val logger = c.value
      logger.debug("CardComp.countAll")
      Set("Edito", "Video").point[Future]
    }

    def rank() = Monitored(Call.Tags.empty) { (c: Call.State[Log]) =>
      val logger = c.value
      logger.debug("CardComp.rank")
      List(1 -> Card("foo"), 1 -> Card("bar")).point[Future]
    }

    def cardsInfos(cs: List[(Int, Card)], pin: Option[Int]) = Monitored(Call.Tags.empty) { (c: Call.State[Log]) =>
      val logger = c.value
      logger.debug("CardComp.cardsInfos")
      List(
        Card("foo") -> List(Community("community 1"), Community("community 2")),
        Card("bar") -> List(Community("community 2"))).point[Future]
    }
  }

  import java.net.URL
  case class Highlight(title: String, cover: URL)
  object HighlightComp {
    def get() = Monitored(Call.Tags.empty) { (c: Call.State[Log]) =>
      val logger = c.value
      logger.debug("HighlightComp.get")
      Highlight("demo", new URL("http://nd04.jxs.cz/641/090/34f0421346_74727174_o2.png")).point[Future]
    }
  }

  it should "pass context" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State[Unit]]()

    def push(state: Call.State[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Monitored(Tags(Callee("f1"))){ (c: Call.State[Unit]) =>
      push(c)
      1.point[Future]
    }

    val (graph, res) = f1.run(nostate).futureValue
    res should ===(1)
    ctxs.length should ===(1)

    ctxs.toList should ===(toStates(graph).toList)
  }

  it should "preserve context on map" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State[Unit]]()

    def push(state: Call.State[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Monitored(Call.Tags.empty){ (c: Call.State[Unit]) =>
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
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State[Unit]]()

    def push(state: Call.State[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Monitored(Tags(Callee("f1"))){ (c: Call.State[Unit]) =>
      push(c)
      1.point[Future]
    }

    def f2(i: Int) = Monitored(Tags(Callee("f2"))){ (c: Call.State[Unit]) =>
      push(c)
      s"foo $i".point[Future]
    }

    def f3(s: String) = Monitored(Tags(Callee("f3"))){ (c: Call.State[Unit]) =>
      push(c)
      s"f3 $s".point[Future]
    }

    val f = Monitored(Tags(Callee("anon0")))(f1
      .flatMap(i => f2(i))
      .flatMap(s => f3(s)))

    val (graph, res) = f.run(nostate).futureValue
    res should ===("f3 foo 1")

    ctxs.length should ===(3)
    ctxs.toList should ===(toStates(graph).toList.drop(1))
  }

  it should "stack contexts" in {
    def f1 = Monitored(Call.Tags.empty){ (c: Call.State[Unit]) =>
      1.point[Future]
    }

    val stacked = Monitored(Call.Tags.empty)(f1)
    val (graph, r) = stacked.run(nostate).futureValue
    r should ===(1)

    graph.children should have length 1
    graph.children.head.children should have length 1
  }

  it should "provide context to C" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State[Unit]]()

    def push(state: Call.State[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Monitored(Call.Tags.empty) { (c: Call.State[Unit]) =>
      push(c)
      1.point[Future]
    }

    def f2(i: Int) = Monitored(Call.Tags.empty){ (c: Call.State[Unit]) =>
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

    val res3 = Monitored(Call.Tags.empty)(f1)
    res3.eval(nostate).futureValue should ===(1)

    ctxs should have length(1)
    ctxs.map(_.span).toSet.size should ===(1) // span is unique
    forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }

    ctxs.clear()

    val res4 = Monitored(Call.Tags.empty) {
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
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State[Unit]]()

    def push(state: Call.State[Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Monitored(Call.Tags.empty) { (c: Call.State[Unit]) =>
      push(c)
      Option(1).point[Future]
    }

    def f2(i: Int) = Monitored(Call.Tags.empty){ (c: Call.State[Unit]) =>
      push(c)
      Option(s"foo $i").point[Future]
    }

    val res4 = Monitored(Call.Tags.empty) {
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

  // it should "real world wb.fr home" in {

  //   val logs = scala.collection.mutable.ArrayBuffer[String]()

  //   case class Logger(state: Call.State) extends Log {
  //     def debug(s: String): Unit = {
  //       logs += s"[DEBUG] ${state.span.value} -> /${state.path.mkString("/")} $s"
  //       ()
  //     }
  //   }

  //   val getPin =
  //     (for {
  //       b   <- trans(BoardComp.get().lift[Option])
  //       id  <- trans(Monitored(Call.Tags.empty)((_: Call[Log]) => b.pin.point[Future]))
  //       pin <- trans(CardComp.getPin(id))
  //     } yield pin).cotrans


  //   val res = for {
  //     pin            <- getPin
  //     cs             <- CardComp.rank()
  //     cards          <- CardComp.cardsInfos(cs, pin.map(_._1))
  //     availableTypes <- CardComp.countAll()
  //     h              <- HighlightComp.get()
  //   } yield (pin, cs, cards, availableTypes, h)


  //   res.eval(state => Logger(state)).futureValue should ===(
  //     (Some((1, Card("card 1"))),
  //       List((1, Card("foo")), (1, Card("bar"))),
  //       List(
  //         (Card("foo"), List(
  //           Community("community 1"),
  //           Community("community 2"))),
  //         (Card("bar"),List(
  //           Community("community 2")))),
  //       Set("Edito", "Video"),
  //       Highlight("demo", new URL("http://nd04.jxs.cz/641/090/34f0421346_74727174_o2.png")))
  //   )
  // }

}
