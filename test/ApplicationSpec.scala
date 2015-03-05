import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

import shapeless.{ HList, ::, HNil }


@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  import monitor.{ Monitored }, Monitored._
  trait Log {
    def debug(s: String): Unit
  }

  "Application" should {
    import play.api.libs.concurrent.Execution.Implicits._
    import scala.concurrent.Future
    import scalaz.std.scalaFuture._
    import scalaz.std.option._
    import scalaz.syntax.monad._
    import scalaz.{ Kleisli, OptionT, EitherT }

    val nocontext: Context.State => HNil = _ => HNil

    "trivial" in {
      def f1 = Monitored.apply0{(_: Context[HNil]) => 1}
      def f2(i: Int) = Monitored.apply0{(_: Context[HNil]) => s"foo $i"}

      val res = for {
        i <- f1
        r <- f2(i)
      } yield r

      res(nocontext) must be_==("foo 1")
    }

    "simple" in {
      def f1 = Monitored{(_: Context[HNil]) => 1.point[Future]}
      def f2(i: Int) = Monitored{(_: Context[HNil]) => s"foo $i".point[Future]}

      val res = for {
        i <- f1
        r <- f2(i)
      } yield r

      res(nocontext) must be_==("foo 1").await
    }

    "optT" in {
      val f1 = Monitored((_: Context[HNil]) => Option("foo").point[Future])
      val f2 = Monitored((_: Context[HNil]) => Option(1).point[Future])
      val f3 = Monitored((_: Context[HNil]) => (None: Option[Int]).point[Future])

      val res = for {
        e1 <- trans(f1)
        e2 <- trans(f2)
      } yield (e1, e2)

      res(nocontext).run must be_==(Some(("foo",1))).await

      val res2 = for {
        e1 <- trans(f1)
        e2 <- trans(f3)
      } yield (e1, e2)

      res2(nocontext).run must be_==(None).await

      val res3 = for {
        e1 <- trans(f3)
        e2 <- trans(f2)
      } yield (e1, e2)

      res3(nocontext).run must be_==(None).await
    }

    "listT" in {
      val f1 = Monitored((_: Context[HNil]) => List("foo", "bar").point[Future])
      val f2 = Monitored((_: Context[HNil]) => List(1, 2).point[Future])
      val f3 = Monitored((_: Context[HNil]) => List[Int]().point[Future])

      val res = for {
        e1 <- trans(f1)
        e2 <- trans(f2)
      } yield (e1, e2)

      res(nocontext).run must be_==(List(("foo",1), ("foo",2), ("bar",1), ("bar",2))).await

      val res2 = for {
        e1 <- trans(f1)
        e2 <- trans(f3)
      } yield (e1, e2)

      res2(nocontext).run must be_==(List()).await

      val res3 = for {
        e1 <- trans(f3)
        e2 <- trans(f2)
      } yield (e1, e2)

      res3(nocontext).run must be_==(List()).await
    }

    "EitherT" in {
      import scalaz.{ \/ , \/-, -\/}
      import EitherT.eitherTFunctor

      val f1: Monitored[HNil, Future, String \/ String] =
        Monitored(_ => \/-("foo").point[Future])
      val f2: Monitored[HNil, Future, String \/ Int] =
        Monitored(_ => \/-(1).point[Future])
      val f3: Monitored[HNil, Future, String \/ String] =
        Monitored(_ => -\/("Error").point[Future])

      type Foo[A] = EitherT[Future, String, A]
      implicitly[scalaz.Functor[Foo]]

      val res = for {
        e1 <- trans(f1)
        e2 <- trans(f2)
      } yield (e1, e2)

      res(nocontext).run must be_==(\/-("foo" -> 1)).await

      val error = -\/("Error")
      val res2 = for {
        e1 <- trans(f1)
        e2 <- trans(f3)
      } yield (e1, e2)

      res2(nocontext).run must be_==(error).await

      val res3 = for {
        e1 <- trans(f3)
        e2 <- trans(f2)
      } yield (e1, e2)

      res3(nocontext).run must be_==(error).await
    }

    case class Board(pin: Option[Int])
    object BoardComp {
      def get() = Monitored { (c: Context[Log :: HNil]) =>
        val logger :: _ = c.value
        logger.debug("BoardComp.get")
        Board(Option(1)).point[Future]
      }
    }

    case class Community(name: String)
    case class Card(name: String)

    object CardComp {
      def getPin(id: Int) = Monitored { (c: Context[Log :: HNil]) =>
        val logger :: _ = c.value
        logger.debug("CardComp.getPin")
        Option(1 -> Card("card 1")).point[Future]
      }

      def countAll() = Monitored { (c: Context[Log :: HNil]) =>
        val logger :: _ = c.value
        logger.debug("CardComp.countAll")
        Set("Edito", "Video").point[Future]
      }

      def rank() = Monitored { (c: Context[Log :: HNil]) =>
        val logger :: _ = c.value
        logger.debug("CardComp.rank")
        List(1 -> Card("foo"), 1 -> Card("bar")).point[Future]
      }

      def cardsInfos(cs: List[(Int, Card)], pin: Option[Int]) = Monitored { (c: Context[Log :: HNil]) =>
        val logger :: _ = c.value
        logger.debug("CardComp.cardsInfos")
        List(
          Card("foo") -> List(Community("community 1"), Community("community 2")),
          Card("bar") -> List(Community("community 2"))).point[Future]
      }
    }

    import java.net.URL
    case class Highlight(title: String, cover: URL)
    object HighlightComp {
      def get() = Monitored { (c: Context[Log :: HNil]) =>
        val logger :: _ = c.value
        logger.debug("HighlightComp.get")
        Highlight("demo", new URL("http://nd04.jxs.cz/641/090/34f0421346_74727174_o2.png")).point[Future]
      }
    }

    "have context" in {
      val ctxs = scala.collection.mutable.ArrayBuffer[Context.State]()

      case class ContextTester(state: Context.State) {
        def push(): Unit = ctxs += state
      }

      def f1 = Monitored{ (c: Context[ContextTester :: HNil]) =>
        val tester :: HNil = c.value
        tester.push()
        1.point[Future]
      }

      f1(s => ContextTester(s) :: HNil) must be_==(1).await
      ctxs.length must be_==(1)
    }

    "preserve context on map" in {
      val ctxs = scala.collection.mutable.ArrayBuffer[Context.State]()

      case class ContextTester(state: Context.State) {
        def push(): Unit = ctxs += state
      }

      def f1 = Monitored{ (c: Context[ContextTester :: HNil]) =>
        val tester :: HNil = c.value
        tester.push()
        1.point[Future]
      }.map(identity).map(identity).map(identity).map(identity)

      f1(s => ContextTester(s) :: HNil) must be_==(1).await

      ctxs.length must be_==(1)
      ctxs.head._2.length must be_==(1)
    }

    "preserve context on flatMap" in {
      val ctxs = scala.collection.mutable.ArrayBuffer[Context.State]()

      case class ContextTester(state: Context.State) {
        def push(name: String): Unit = ctxs += state
      }

      def f1 = Monitored{ (c: Context[ContextTester :: HNil]) =>
        val tester :: HNil = c.value
        tester.push("f1")
        1.point[Future]
      }

      def f2(i: Int) = Monitored{ (c: Context[ContextTester :: HNil]) =>
        val tester :: HNil = c.value
        tester.push("f2")
        s"foo $i".point[Future]
      }

      def f3(s: String) = Monitored{ (c: Context[ContextTester :: HNil]) =>
        val tester :: HNil = c.value
        tester.push("f3")
        s"f3 $s".point[Future]
      }

      val f = f1
        .flatMap(i => f2(i))
        .flatMap(s => f3(s))

      f(s => ContextTester(s) :: HNil) must be_==("f3 foo 1").await

      ctxs.length must be_==(3)
    }

    "stack contexts" in {
      val ctxs = scala.collection.mutable.ArrayBuffer[Context.State]()

      case class ContextTester(state: Context.State) {
        def push(): Unit = ctxs += state
      }

      def f1 = Monitored{ (c: Context[ContextTester :: HNil]) =>
        val tester :: HNil = c.value
        tester.push()
        1.point[Future]
      }

      val stacked = Monitored(f1)
      stacked(s => ContextTester(s) :: HNil) must be_==(1).await
      ctxs.length must be_==(1)
      ctxs.head._2.length must be_==(2)
    }

    "provide context to C" in {
      val ctxs = scala.collection.mutable.ArrayBuffer[Context.State]()

      case class ContextTester(state: Context.State) {
        def push(): Unit = ctxs += state
      }

      def f1 = Monitored { (c: Context[ContextTester :: HNil]) =>
        val tester :: HNil = c.value
        tester.push()
        1.point[Future]
      }

      def f2(i: Int) = Monitored{ (c: Context[ContextTester :: HNil]) =>
        val tester :: HNil = c.value
        tester.push()
        s"foo $i".point[Future]
      }

      f1(s => ContextTester(s) :: HNil) must be_==(1).await
      ctxs.length must be_==(1)
      ctxs.head._2.length must be_==(1)


      ctxs.clear()

      val res2 = f1.map(identity)
      res2(s => ContextTester(s) :: HNil)

      ctxs must have length(1)
      ctxs.map(_._2.length == 1) must (beTrue).foreach


      ctxs.clear()

      val res = for {
        i <- f1
        r <- f2(i)
      } yield r

      res(s => ContextTester(s) :: HNil) must be_==("foo 1").await

      ctxs must have length(2)
      ctxs.map(_._1).toSet must have length(1) // span is unique
      ctxs.map(_._2.length == 1) must (beTrue).foreach

      ctxs.clear()

      val res3 = Monitored(f1)
      res3(s => ContextTester(s) :: HNil) must be_==(1).await

      ctxs must have length(1)
      ctxs.map(_._1).toSet must have length(1) // span is unique
      ctxs.map(_._2.length == 2) must (beTrue).foreach

      ctxs.clear()

      val res4 = Monitored {
        for {
          i <- f1
          r <- f2(i)
        } yield r
      }

      res4(s => ContextTester(s) :: HNil) must be_==("foo 1").await

      ctxs must have length(2)
      ctxs.map(_._1).toSet must have length(1) // span is unique
      ctxs.map(_._2.length == 2) must (beTrue).foreach
    }

    "real world wb.fr home" in {

      val logs = scala.collection.mutable.ArrayBuffer[String]()

      case class Logger(state: Context.State) extends Log {
        def debug(s: String): Unit = logs += s"[DEBUG] ${state._1.value} -> /${state._2.map(_.value).mkString("/")} $s"
      }

      val getPin =
        (for {
          b   <- trans(BoardComp.get().lift[Option])
          id  <- trans(Monitored((_: Context[Log :: HNil]) => b.pin.point[Future]))
          pin <- trans(CardComp.getPin(id))
        } yield pin).run


      val res = for {
        pin            <- getPin
        cs             <- CardComp.rank()
        cards          <- CardComp.cardsInfos(cs, pin.map(_._1))
        availableTypes <- CardComp.countAll()
        h              <- HighlightComp.get()
      } yield (pin, cs, cards, availableTypes, h)


      res(state => Logger(state) :: HNil) must be_==(
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
      ).await

      ok


      // logs must be_==(Seq(
      //   "[DEBUG] BoardComp.get",
      //   "[DEBUG] CardComp.getPin",
      //   "[DEBUG] CardComp.rank",
      //   "[DEBUG] CardComp.cardsInfos",
      //   "[DEBUG] CardComp.countAll",
      //   "[DEBUG] HighlightComp.get"))
    }
  }
}
