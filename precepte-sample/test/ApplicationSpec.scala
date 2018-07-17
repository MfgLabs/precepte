import play.api.test._

class ApplicationSpec extends PlaySpecification {
  // -- Date helpers

  def dateIs(date: java.util.Date, str: String) = new java.text.SimpleDateFormat("yyyy-MM-dd").format(date) == str

  // --

  "Application" should {

    "redirect to the computer list on /" in new WithApplication {
      val result = Controllers.application.index(FakeRequest())

      status(result) must equalTo(SEE_OTHER)
      redirectLocation(result) must beSome.which(_ == "/computers")
    }

    "list computers on the the first page" in new WithApplication {
      val result = Controllers.application.list(0, 2, "")(FakeRequest())

      status(result) must equalTo(OK)
      contentAsString(result) must contain("574 computers found")
    }

    "filter computer by name" in new WithApplication {
      val result = Controllers.application.list(0, 2, "Apple")(FakeRequest())

      status(result) must equalTo(OK)
      contentAsString(result) must contain("13 computers found")
    }

    "create new computer" in new WithApplication {
      val badResult = Controllers.application.save(FakeRequest())

      status(badResult) must equalTo(BAD_REQUEST)

      val badDateFormat = Controllers.application.save(
        FakeRequest().withFormUrlEncodedBody("name" -> "FooBar", "introduced" -> "badbadbad", "company" -> "1")
      )

      status(badDateFormat) must equalTo(BAD_REQUEST)
      contentAsString(badDateFormat) must contain("""<option value="1" selected="selected">Apple Inc.</option>""")
      contentAsString(badDateFormat) must contain("""<input type="text" id="introduced" name="introduced" value="badbadbad" />""")
      contentAsString(badDateFormat) must contain("""<input type="text" id="name" name="name" value="FooBar" />""")

      val result = Controllers.application.save(
        FakeRequest().withFormUrlEncodedBody("name" -> "FooBar", "introduced" -> "2011-12-24", "company" -> "1")
      )

      status(result) must equalTo(SEE_OTHER)
      redirectLocation(result) must beSome.which(_ == "/computers")
      flash(result).get("success") must beSome.which(_ == "Computer FooBar has been created")

      val list = Controllers.application.list(0, 2, "FooBar")(FakeRequest())

      status(list) must equalTo(OK)
      contentAsString(list) must contain("One computer found")
    }

  }

}
