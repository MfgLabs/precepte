import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._


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

    "Simple" in {
      def f1 = Monitored.apply0{(_: Log) => 1}
      def f2(i: Int) = Monitored.apply0{(_: Log) => s"foo $i"}

      val res = for {
        i <- f1
        r <- f2(i)
      } yield r

      res(null) must be_==("foo 1")
    }

    "optT" in {
      val f1 = Monitored((_: Log) => Option("foo").point[Future])
      val f2 = Monitored((_: Log) => Option(1).point[Future])
      val f3 = Monitored((_: Log) => (None: Option[Int]).point[Future])

      val res = for {
        e1 <- trans(f1)
        e2 <- trans(f2)
      } yield (e1, e2)

      res(null).run must be_==(Some(("foo",1))).await

      val res2 = for {
        e1 <- trans(f1)
        e2 <- trans(f3)
      } yield (e1, e2)

      res2(null).run must be_==(None).await

      val res3 = for {
        e1 <- trans(f3)
        e2 <- trans(f2)
      } yield (e1, e2)

      res3(null).run must be_==(None).await
    }

    // "listT" in {
    //   val f1 = Monitored((_: Context[Log]) => List("foo", "bar").point[Future])
    //   val f2 = Monitored((_: Context[Log]) => List(1, 2).point[Future])
    //   val f3 = Monitored((_: Context[Log]) => List[Int]().point[Future])

    //   val res = for {
    //     e1 <- f1.T
    //     e2 <- f2.T
    //   } yield (e1, e2)

    //   res.run(null).run must be_==(List(("foo",1), ("foo",2), ("bar",1), ("bar",2))).await

    //   val res2 = for {
    //     e1 <- f1.T
    //     e2 <- f3.T
    //   } yield (e1, e2)

    //   res2.run(null).run must be_==(List()).await

    //   val res3 = for {
    //     e1 <- f3.T
    //     e2 <- f2.T
    //   } yield (e1, e2)

    //   res3.run(null).run must be_==(List()).await
    // }

    // "EitherT" in {
    //   import scalaz.{ \/ , \/-, -\/}
    //   import EitherT.eitherTFunctor

    //   val f1: Monitored[Log, Future[String \/ String]] =
    //     Monitored(_ => \/-("foo").point[Future])
    //   val f2: Monitored[Log, Future[String \/ Int]] =
    //     Monitored(_ => \/-(1).point[Future])
    //   val f3: Monitored[Log, Future[String \/ String]] =
    //     Monitored(_ => -\/("Error").point[Future])

    //   val res = for {
    //     e1 <- f1.T
    //     e2 <- f2.T
    //   } yield (e1, e2)

    //   res.run(null).run must be_==(\/-("foo" -> 1)).await

    //   val error = -\/("Error")
    //   val res2 = for {
    //     e1 <- f1.T
    //     e2 <- f3.T
    //   } yield (e1, e2)

    //   res2.run(null).run must be_==(error).await

    //   val res3 = for {
    //     e1 <- f3.T
    //     e2 <- f2.T
    //   } yield (e1, e2)

    //   res3.run(null).run must be_==(error).await
    // }

    // case class Board(pin: Option[Int])
    // object BoardComp {
    //   def get(): Monitored[Log, Future[Board]] = Monitored{ c =>
    //     c.value.debug("BoardComp.get")
    //     Board(Some(1)).point[Future]
    //   }
    // }

    // case class Community(name: String)
    // case class Card(name: String)

    // object CardComp {
    //   def getPin(id: Int): Monitored[Log, Future[Option[(Int, Card)]]] = Monitored { c =>
    //     c.value.debug("CardComp.getPin")
    //     Some(1 -> Card("card 1")).point[Future]
    //   }

    //   def countAll(): Monitored[Log, Future[Set[String]]] = Monitored { c =>
    //     c.value.debug("CardComp.countAll")
    //     Set("Edito", "Video").point[Future]
    //   }

    //   def rank(): Monitored[Log, Future[List[(Int, Card)]]] = Monitored { c =>
    //     c.value.debug("CardComp.rank")
    //     List(1 -> Card("foo"), 1 -> Card("bar")).point[Future]
    //   }

    //   def cardsInfos(cs: List[(Int, Card)], pin: Option[Int]): Monitored[Log, Future[List[(Card, List[Community])]]] = Monitored { c =>
    //     c.value.debug("CardComp.cardsInfos")
    //     List(
    //       Card("foo") -> List(Community("community 1"), Community("community 2")),
    //       Card("bar") -> List(Community("community 2"))).point[Future]
    //   }
    // }

    // import java.net.URL
    // case class Highlight(title: String, cover: URL)
    // object HighlightComp {
    //   def get(): Monitored[Log, Future[Highlight]] = Monitored { c =>
    //     c.value.debug("HighlightComp.get")
    //     Highlight("demo", new URL("http://nd04.jxs.cz/641/090/34f0421346_74727174_o2.png")).point[Future]
    //   }
    // }


    // "real world wb.fr home" in {

    //   val logs = scala.collection.mutable.ArrayBuffer[String]()

    //   val logger = new Log {
    //     def debug(s: String): Unit = logs += s"[DEBUG] $s"
    //   }

    //   val context = Context(logger)

    //   val getPin =
    //     (for {
    //       b <- BoardComp.get().lift[Option].T
    //       id <- Monitored((_: Context[Log]) => b.pin.point[Future]).T
    //       pin <- CardComp.getPin(id).T
    //     } yield pin).run


    //   val res = for {
    //     pin <- Monitored(getPin(_: Context[Log]).run).K
    //     cs <- CardComp.rank().K
    //     cards <- CardComp.cardsInfos(cs, pin.map(_._1)).K
    //     availableTypes <- CardComp.countAll().K
    //     h <- HighlightComp.get().K
    //   } yield (pin, cs, cards, availableTypes, h)

    //   res.run(context) must be_==(
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
    //   ).await

    //   logs must be_==(Seq(
    //     "[DEBUG] BoardComp.get",
    //     "[DEBUG] CardComp.getPin",
    //     "[DEBUG] CardComp.rank",
    //     "[DEBUG] CardComp.cardsInfos",
    //     "[DEBUG] CardComp.countAll",
    //     "[DEBUG] HighlightComp.get"))
    // }
  }
}
