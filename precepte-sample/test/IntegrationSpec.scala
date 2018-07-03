import org.specs2.runner._
import org.junit.runner._

import play.api.test._

import org.fluentlenium.core.filter.FilterConstructor._

@RunWith(classOf[JUnitRunner])
class IntegrationSpec extends PlaySpecification {

  "Application" should {

    "work from within a browser" in new WithApplication { new WithBrowser(webDriver = WebDriverFactory(HTMLUNIT), app = app) {
        browser.goTo("http://localhost:19001/")

        browser.$("header h1").first.text must equalTo("Play sample application — Computer database")
        browser.$("section h1").first.text must equalTo("574 computers found")

        browser.$("#pagination li.current").first.text must equalTo("Displaying 1 to 10 of 574")

        browser.$("#pagination li.next a").click()

        browser.$("#pagination li.current").first.text must equalTo("Displaying 11 to 20 of 574")
        browser.$("#searchbox").write("Apple")
        browser.$("#searchsubmit").click()

        browser.$("section h1").first.text must equalTo("13 computers found")
        browser.$("a", withText("Apple II")).click()

        browser.$("section h1").first.text must equalTo("Edit computer")

        browser.$("#discontinued").write("xxx")
        browser.$("input.primary").click()

        browser.$("div.error").size must equalTo(1)
        browser.$("div.error label").first.text must equalTo("Discontinued date")

        browser.$("#discontinued").write("")
        browser.$("input.primary").click()

        browser.$("section h1").first.text must equalTo("574 computers found")
        browser.$(".alert-message").first.text must equalTo("Done! Computer Apple II has been updated")

        browser.$("#searchbox").write("Apple")
        browser.$("#searchsubmit").click()

        browser.$("a", withText("Apple II")).click()
        browser.$("input.danger").click()

        browser.$("section h1").first.text must equalTo("573 computers found")
        browser.$(".alert-message").first.text must equalTo("Done! Computer has been deleted")

        browser.$("#searchbox").write("Apple")
        browser.$("#searchsubmit").click()

        browser.$("section h1").first.text must equalTo("12 computers found")

      }
    }
  }
}
