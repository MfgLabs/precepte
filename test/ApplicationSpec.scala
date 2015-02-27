import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  "Application" should {
    import play.api.libs.concurrent.Execution.Implicits._
    import scala.concurrent.Future
    import scalaz.std.scalaFuture._
    import scalaz.std.option._
    import scalaz.syntax.applicative._
    import scalaz.{ Kleisli, OptionT }
    import monitor.Monitored, Monitored._

    "optT" in {
      val f1 = Monitored(_ => Option("foo").point[Future])
      val f2 = Monitored(_ => Option(1).point[Future])
      val f3 = Monitored(_ => (None: Option[Int]).point[Future])

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
      val f1 = Monitored(_ => List("foo", "bar").point[Future])
      val f2 = Monitored(_ => List(1, 2).point[Future])
      val f3 = Monitored(_ => List[Int]().point[Future])

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

    case class Board(pin: Option[Int])
    object BoardComp {
      def get(): Monitored[Future[Board]] =
        Monitored{ _ => Board(Some(1)).point[Future] }
    }

    case class Community(name: String)
    case class Card(name: String)

    object CardComp {
      def getPin(id: Int): Monitored[Future[Option[(Int, Card)]]] =
        Monitored { _ => Some(1 -> Card("card 1")).point[Future] }

      def countAll(): Monitored[Future[Set[String]]] =
        Monitored { _ => Set("Edito", "Video").point[Future] }

      def rank(): Monitored[Future[Seq[(Int, Card)]]] =
        Monitored { _ => Seq(1 -> Card("foo"), 1 -> Card("bar")).point[Future] }

      def cardsInfos(cs: Seq[(Int, Card)], pin: Option[Int]): Monitored[Future[Seq[(Card, Seq[Community])]]] = Monitored { _ =>
        Seq(
          Card("foo") -> Seq(Community("community 1"), Community("community 2")),
          Card("bar") -> Seq(Community("community 2"))).point[Future]
      }
    }

    import java.net.URL
    case class Highlight(title: String, cover: URL)
    object HighlightComp {
      def get(): Monitored[Future[Highlight]] =
        Monitored { _ => Highlight("demo", new URL("http://nd04.jxs.cz/641/090/34f0421346_74727174_o2.png")).point[Future] }
    }


    "real world" in {

      val getPin =
        (for {
          b <- BoardComp.get().lift[Option].T
          id <- Monitored(_ => b.pin.point[Future]).T
          pin <- CardComp.getPin(id).T
        } yield pin).run


      // for {
      //   pin <- getPin
      //   cs <- card.rank(request.ctx.creds.map(_._1), pagination, types)
      //   cards <- card.cardsInfos(cs, pin.map(_._1), cacheStrat)
      //   availableTypes <- count
      //   h <- high
      // } yield (pin, cs, cards, availableTypes, h)
      ok
    }

  }
}
