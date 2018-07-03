import org.specs2.mutable._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class ModelSpec extends Specification {

  import models._
  import commons.Monitoring._

  // -- Date helpers

  def dateIs(date: java.util.Date, str: String) = new java.text.SimpleDateFormat("yyyy-MM-dd").format(date) == str

  // --
  import com.mfglabs.precepte._
  import default._
  import corescalaz._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scalaz.std.scalaFuture._

  def nostate: ST[Unit] =
    ST(
      Span.gen,
      BaseEnv(Host("localhost"), Environment.Dev, Version("1.0")),
      Vector.empty,
      ()
    )

  "Computer model" should {

    "be retrieved by id" in new WithApplication {
      val Some(macintosh) = Await.result(Services.computerDB.findById(21).eval(nostate), Duration.Inf)

      macintosh.name must equalTo("Macintosh")
      macintosh.introduced must beSome.which(dateIs(_, "1984-01-24"))
    }

    "be listed along its companies" in new WithApplication {
      val computers =  Await.result(Services.computerDB.list().eval(nostate), Duration.Inf)

      computers.total must equalTo(574)
      computers.items must have length(10)
    }

    "be updated if needed" in new WithApplication {
      Await.result(
        Services.computerDB.update(21, Computer(name="The Macintosh", introduced=None, discontinued=None, companyId=Some(1))).eval(nostate),
        Duration.Inf
      )

      val Some(macintosh) = Await.result(Services.computerDB.findById(21).eval(nostate), Duration.Inf)

      macintosh.name must equalTo("The Macintosh")
      macintosh.introduced must beNone
    }

  }

}
