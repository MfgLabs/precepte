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

  }
}
