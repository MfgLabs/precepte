package com.mfglab.monitoring

import org.scalatest._
import Matchers._
import Inspectors._

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

  val nocontext: Call.State => Unit = _ => ()

  "Monitored" should  "trivial" in {
    def f1 = Monitored(Call.Tags.empty).apply0{(_: Call[Unit]) => 1}
    def f2(i: Int) = Monitored(Call.Tags.empty).apply0{(_: Call[Unit]) => s"foo $i"}

    f1.eval(nocontext) should ===(1)

    val res = for {
      i <- f1
      r <- f2(i)
    } yield r

    res.eval(nocontext) should ===("foo 1")
  }

  it should "simple" in {
    def f1 = Monitored(Call.Tags.empty){(_: Call[Unit]) => 1.point[Future]}
    def f2(i: Int) = Monitored(Call.Tags.empty){(_: Call[Unit]) => s"foo $i".point[Future]}

    val res = for {
      i <- f1
      r <- f2(i)
    } yield r

    res.eval(nocontext).futureValue should ===("foo 1")
  }

  it should "optT" in {
    val f1 = Monitored(Call.Tags.empty)((_: Call[Unit]) => Option("foo").point[Future])
    val f2 = Monitored(Call.Tags.empty)((_: Call[Unit]) => Option(1).point[Future])
    val f3 = Monitored(Call.Tags.empty)((_: Call[Unit]) => (None: Option[Int]).point[Future])

    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.eval(nocontext).run.futureValue should ===(Some(("foo",1)))

    val res2 = for {
      e1 <- trans(f1)
      e2 <- trans(f3)
    } yield (e1, e2)

    res2.eval(nocontext).run.futureValue should ===(None)

    val res3 = for {
      e1 <- trans(f3)
      e2 <- trans(f2)
    } yield (e1, e2)

    res3.eval(nocontext).run.futureValue should ===(None)
  }

  it should "listT" in {
    val f1 = Monitored(Call.Tags.empty)((_: Call[Unit]) => List("foo", "bar").point[Future])
    val f2 = Monitored(Call.Tags.empty)((_: Call[Unit]) => List(1, 2).point[Future])
    val f3 = Monitored(Call.Tags.empty)((_: Call[Unit]) => List[Int]().point[Future])

    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.eval(nocontext).run.futureValue should ===(List(("foo",1), ("foo",2), ("bar",1), ("bar",2)))

    val res2 = for {
      e1 <- trans(f1)
      e2 <- trans(f3)
    } yield (e1, e2)

    res2.eval(nocontext).run.futureValue should ===(List())

    val res3 = for {
      e1 <- trans(f3)
      e2 <- trans(f2)
    } yield (e1, e2)

    res3.eval(nocontext).run.futureValue should ===(List())
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
    implicitly[scalaz.Functor[Foo]]

    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.eval(nocontext).run.futureValue should ===(\/-("foo" -> 1))

    val error = -\/("Error")
    val res2 = for {
      e1 <- trans(f1)
      e2 <- trans(f3)
    } yield (e1, e2)

    res2.eval(nocontext).run.futureValue should ===(error)

    val res3 = for {
      e1 <- trans(f3)
      e2 <- trans(f2)
    } yield (e1, e2)

    res3.eval(nocontext).run.futureValue should ===(error)
  }

  case class Board(pin: Option[Int])
  object BoardComp {
    def get() = Monitored(Call.Tags.empty) { (c: Call[Log]) =>
      val logger = c.value
      logger.debug("BoardComp.get")
      Board(Option(1)).point[Future]
    }
  }

  case class Community(name: String)
  case class Card(name: String)

  object CardComp {
    def getPin(id: Int) = Monitored(Call.Tags.empty) { (c: Call[Log]) =>
      val logger = c.value
      logger.debug("CardComp.getPin")
      Option(1 -> Card("card 1")).point[Future]
    }

    def countAll() = Monitored(Call.Tags.empty) { (c: Call[Log]) =>
      val logger = c.value
      logger.debug("CardComp.countAll")
      Set("Edito", "Video").point[Future]
    }

    def rank() = Monitored(Call.Tags.empty) { (c: Call[Log]) =>
      val logger = c.value
      logger.debug("CardComp.rank")
      List(1 -> Card("foo"), 1 -> Card("bar")).point[Future]
    }

    def cardsInfos(cs: List[(Int, Card)], pin: Option[Int]) = Monitored(Call.Tags.empty) { (c: Call[Log]) =>
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
    def get() = Monitored(Call.Tags.empty) { (c: Call[Log]) =>
      val logger = c.value
      logger.debug("HighlightComp.get")
      Highlight("demo", new URL("http://nd04.jxs.cz/641/090/34f0421346_74727174_o2.png")).point[Future]
    }
  }

  it should "have context" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State]()

    case class ContextTester(state: Call.State) {
      def push(): Unit = {
        ctxs += state
        ()
      }
    }

    def f1 = Monitored(Call.Tags.empty){ (c: Call[ContextTester]) =>
      val tester = c.value
      tester.push()
      1.point[Future]
    }

    f1.eval(s => ContextTester(s)).futureValue should ===(1)
    ctxs.length should ===(1)
  }

  it should "preserve context on map" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State]()

    case class ContextTester(state: Call.State) {
      def push(): Unit = {
        ctxs += state
        ()
      }
    }

    def f1 = Monitored(Call.Tags.empty){ (c: Call[ContextTester]) =>
      val tester = c.value
      tester.push()
      1.point[Future]
    }.map(identity).map(identity).map(identity).map(identity)

    f1.eval(s => ContextTester(s)).futureValue should ===(1)

    ctxs.length should ===(1)
    ctxs.head.path.length should ===(1)
  }

  it should "preserve context on flatMap" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State]()

    case class ContextTester(state: Call.State) {
      def push(name: String): Unit = {
        ctxs += state
        ()
      }
    }

    def f1 = Monitored(Call.Tags.empty){ (c: Call[ContextTester]) =>
      val tester = c.value
      tester.push("f1")
      1.point[Future]
    }

    def f2(i: Int) = Monitored(Call.Tags.empty){ (c: Call[ContextTester]) =>
      val tester = c.value
      tester.push("f2")
      s"foo $i".point[Future]
    }

    def f3(s: String) = Monitored(Call.Tags.empty){ (c: Call[ContextTester]) =>
      val tester = c.value
      tester.push("f3")
      s"f3 $s".point[Future]
    }

    val f = Monitored(Call.Tags.empty)(f1
      .flatMap(i => f2(i))
      .flatMap(s => f3(s)))

    f.eval(s => ContextTester(s)).futureValue should ===("f3 foo 1")

    ctxs.length should ===(3)

  }

  it should "stack contexts" in {

    def f1 = Monitored(Call.Tags.empty){ (c: Call[Unit]) =>
      1.point[Future]
    }

    val stacked = Monitored(Call.Tags.empty)(f1)
    val (s, r) = stacked.run(nocontext).futureValue
    r should ===(1)

    s.children should ===(1)
  }

  // it should "provide context to C" in {
  //   val ctxs = scala.collection.mutable.ArrayBuffer[Call.State]()

  //   case class ContextTester(state: Call.State) {
  //     def push(): Unit = {
  //       ctxs += state
  //       ()
  //     }
  //   }

  //   def f1 = Monitored(Call.Tags.empty) { (c: Call[ContextTester]) =>
  //     val tester = c.value
  //     tester.push()
  //     1.point[Future]
  //   }

  //   def f2(i: Int) = Monitored(Call.Tags.empty){ (c: Call[ContextTester]) =>
  //     val tester = c.value
  //     tester.push()
  //     s"foo $i".point[Future]
  //   }

  //   f1.eval(s => ContextTester(s)).futureValue should ===(1)
  //   ctxs.length should ===(1)
  //   ctxs.head.path.length should ===(1)


  //   ctxs.clear()

  //   val res2 = f1.map(identity)
  //   res2.eval(s => ContextTester(s))

  //   ctxs should have length(1)
  //   forAll(ctxs.map(_.path.length == 1)){_  should ===(true) }


  //   ctxs.clear()

  //   val res = for {
  //     i <- f1
  //     r <- f2(i)
  //   } yield r

  //   res.eval(s => ContextTester(s)).futureValue should ===("foo 1")

  //   ctxs should have length(2)
  //   ctxs.map(_.span).toSet.size should ===(1) // span is unique
  //   forAll(ctxs.map(_.path.length == 1)){ _ should ===(true) }

  //   ctxs.clear()

  //   val res3 = Monitored(Call.Tags.empty)(f1)
  //   res3.eval(s => ContextTester(s)).futureValue should ===(1)

  //   ctxs should have length(1)
  //   ctxs.map(_.span).toSet.size should ===(1) // span is unique
  //   forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }

  //   ctxs.clear()

  //   val res4 = Monitored(Call.Tags.empty) {
  //     for {
  //       i <- f1
  //       r <- f2(i)
  //     } yield r
  //   }

  //   res4.eval(s => ContextTester(s)).futureValue should ===("foo 1")

  //   ctxs should have length(2)
  //   ctxs.map(_.span).toSet.size should ===(1) // span is unique
  //   forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }
  // }

  // it should "not stack context on trans" in {
  //   val ctxs = scala.collection.mutable.ArrayBuffer[Call.State]()

  //   case class ContextTester(state: Call.State) {
  //     def push(): Unit = {
  //       ctxs += state
  //       ()
  //     }
  //   }

  //   def f1 = Monitored(Call.Tags.empty) { (c: Call[ContextTester]) =>
  //     val tester = c.value
  //     tester.push()
  //     Option(1).point[Future]
  //   }

  //   def f2(i: Int) = Monitored(Call.Tags.empty){ (c: Call[ContextTester]) =>
  //     val tester = c.value
  //     tester.push()
  //     Option(s"foo $i").point[Future]
  //   }

  //   type X[T] = scalaz.OptionT[Future, T]
  //   val res4 = Monitored(Call.Tags.empty)[ContextTester, X, String] { //TODO: why do I need to force the type
  //     for {
  //       i <- trans(f1)
  //       r <- trans(f2(i))
  //     } yield r
  //   }

  //   res4.eval(s => ContextTester(s)).run.futureValue should ===(Some("foo 1"))

  //   ctxs should have length(2)
  //   ctxs.map(_.span).toSet.size should ===(1) // span is unique
  //   forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }

  // }

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
