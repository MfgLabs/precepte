import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  trait Log {
    def debug(s: String): Unit
  }
  case class Context(logger: Log)

  "Application" should {
    import play.api.libs.concurrent.Execution.Implicits._
    import scala.concurrent.Future
    import scalaz.std.scalaFuture._
    import scalaz.std.option._
    import scalaz.syntax.monad._
    import scalaz.{ Kleisli, OptionT, EitherT }
    import monitor.Monitored, Monitored._

    "Simple" in {
      def f1 = Monitored{(_: Context) => 1}
      def f2(i: Int) = Monitored{(_: Context) => s"foo $i"}

      val res = for {
        i <- f1
        r <- f2(i)
      } yield r

      res.run(null) must be_==("foo 1")
    }

    "optT" in {
      val f1 = Monitored((_: Context) => Option("foo").point[Future])
      val f2 = Monitored((_: Context) => Option(1).point[Future])
      val f3 = Monitored((_: Context) => (None: Option[Int]).point[Future])

      val res = for {
        e1 <- f1.T
        e2 <- f2.T
      } yield (e1, e2)

      res.run(null).run must be_==(Some(("foo",1))).await

      val res2 = for {
        e1 <- f1.T
        e2 <- f3.T
      } yield (e1, e2)

      res2.run(null).run must be_==(None).await

      val res3 = for {
        e1 <- f3.T
        e2 <- f2.T
      } yield (e1, e2)

      res3.run(null).run must be_==(None).await
    }

    "listT" in {
      val f1 = Monitored((_: Context) => List("foo", "bar").point[Future])
      val f2 = Monitored((_: Context) => List(1, 2).point[Future])
      val f3 = Monitored((_: Context) => List[Int]().point[Future])

      val res = for {
        e1 <- f1.T
        e2 <- f2.T
      } yield (e1, e2)

      res.run(null).run must be_==(List(("foo",1), ("foo",2), ("bar",1), ("bar",2))).await

      val res2 = for {
        e1 <- f1.T
        e2 <- f3.T
      } yield (e1, e2)

      res2.run(null).run must be_==(List()).await

      val res3 = for {
        e1 <- f3.T
        e2 <- f2.T
      } yield (e1, e2)

      res3.run(null).run must be_==(List()).await
    }

    "EitherT" in {
      import scalaz.{ \/ , \/-, -\/}
      import EitherT.eitherTFunctor

      val f1: Monitored[Context, Future[String \/ String]] =
        Monitored(_ => \/-("foo").point[Future])
      val f2: Monitored[Context, Future[String \/ Int]] =
        Monitored(_ => \/-(1).point[Future])
      val f3: Monitored[Context, Future[String \/ String]] =
        Monitored(_ => -\/("Error").point[Future])

      val res = for {
        e1 <- f1.T
        e2 <- f2.T
      } yield (e1, e2)

      res.run(null).run must be_==(\/-("foo" -> 1)).await

      val error = -\/("Error")
      val res2 = for {
        e1 <- f1.T
        e2 <- f3.T
      } yield (e1, e2)

      res2.run(null).run must be_==(error).await

      val res3 = for {
        e1 <- f3.T
        e2 <- f2.T
      } yield (e1, e2)

      res3.run(null).run must be_==(error).await
    }

    case class Board(pin: Option[Int])
    object BoardComp {
      def get(): Monitored[Context, Future[Board]] = Monitored{ m =>
        m.logger.debug("BoardComp.get")
        Board(Some(1)).point[Future]
      }
    }

    case class Community(name: String)
    case class Card(name: String)

    object CardComp {
      def getPin(id: Int): Monitored[Context, Future[Option[(Int, Card)]]] = Monitored { m =>
        m.logger.debug("CardComp.getPin")
        Some(1 -> Card("card 1")).point[Future]
      }

      def countAll(): Monitored[Context, Future[Set[String]]] = Monitored { m =>
        m.logger.debug("CardComp.countAll")
        Set("Edito", "Video").point[Future]
      }

      def rank(): Monitored[Context, Future[List[(Int, Card)]]] = Monitored { m =>
        m.logger.debug("CardComp.rank")
        List(1 -> Card("foo"), 1 -> Card("bar")).point[Future]
      }

      def cardsInfos(cs: List[(Int, Card)], pin: Option[Int]): Monitored[Context, Future[List[(Card, List[Community])]]] = Monitored { m =>
        m.logger.debug("CardComp.cardsInfos")
        List(
          Card("foo") -> List(Community("community 1"), Community("community 2")),
          Card("bar") -> List(Community("community 2"))).point[Future]
      }
    }

    import java.net.URL
    case class Highlight(title: String, cover: URL)
    object HighlightComp {
      def get(): Monitored[Context, Future[Highlight]] = Monitored { m =>
        m.logger.debug("HighlightComp.get")
        Highlight("demo", new URL("http://nd04.jxs.cz/641/090/34f0421346_74727174_o2.png")).point[Future]
      }
    }


    "real world wb.fr home" in {

      val logs = scala.collection.mutable.ArrayBuffer[String]()

      val logger = new Log {
        def debug(s: String): Unit = logs += s"[DEBUG] $s"
      }

      val context = Context(logger)

      val getPin =
        (for {
          b <- BoardComp.get().lift[Option].T
          id <- Monitored((_: Context) => b.pin.point[Future]).T
          pin <- CardComp.getPin(id).T
        } yield pin).run


      val res = for {
        pin <- Monitored(getPin(_: Context).run).K
        cs <- CardComp.rank().K
        cards <- CardComp.cardsInfos(cs, pin.map(_._1)).K
        availableTypes <- CardComp.countAll().K
        h <- HighlightComp.get().K
      } yield (pin, cs, cards, availableTypes, h)

      res.run(context) must be_==(
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

      logs must be_==(Seq("[DEBUG] BoardComp.get", "[DEBUG] CardComp.getPin", "[DEBUG] CardComp.rank", "[DEBUG] CardComp.cardsInfos", "[DEBUG] CardComp.countAll", "[DEBUG] HighlightComp.get"))
    }


  }
}
