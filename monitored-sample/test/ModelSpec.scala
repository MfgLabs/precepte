import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

@RunWith(classOf[JUnitRunner])
class ModelSpec extends PlaySpecification {

  import models._

  // -- Date helpers

  def dateIs(date: java.util.Date, str: String) = new java.text.SimpleDateFormat("yyyy-MM-dd").format(date) == str

  // --
  import com.mfglabs.monitoring.Call
  import scala.concurrent.ExecutionContext.Implicits.global
  import scalaz.std.scalaFuture._

  val env = Call.BaseEnv(Call.Tags.Host("localhost"), Call.Tags.Environment.Dev, Call.Tags.Version("1.0"))
  private def tags(n: String) = Call.BaseTags(Call.Tags.Callee(n), Call.Tags.Category.Database)
  def nostate = Call.State[Call.BaseEnv, Call.BaseTags, Unit](Call.Span.gen, env, Vector.empty, ())

  "Computer model" should {

    "be retrieved by id" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {

        val Some(macintosh) = await(Computer.findById(21).eval(nostate))

        macintosh.name must equalTo("Macintosh")
        macintosh.introduced must beSome.which(dateIs(_, "1984-01-24"))

      }
    }

    "be listed along its companies" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {

        val computers =  await(Computer.list().eval(nostate))

        computers.total must equalTo(574)
        computers.items must have length(10)

      }
    }

    "be updated if needed" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {

        await(Computer.update(21, Computer(name="The Macintosh", introduced=None, discontinued=None, companyId=Some(1))).eval(nostate))

        val Some(macintosh) = await(Computer.findById(21).eval(nostate))

        macintosh.name must equalTo("The Macintosh")
        macintosh.introduced must beNone

      }
    }

  }

}