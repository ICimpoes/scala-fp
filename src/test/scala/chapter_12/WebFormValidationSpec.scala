package chapter_12

import java.text.SimpleDateFormat

import org.scalatest.{FlatSpec, Inside, Matchers}
import chapter_12.WebForm._

class WebFormValidationSpec extends FlatSpec with Matchers with Inside {

  "WebForm.validWebForm" should "return Failure containing all failures" in {

    inside(validWebForm("", "aaa", "1212")) {
      case Failure(h, t) =>
        h shouldBe invalidNameMsg
        t should contain theSameElementsAs Seq(invalidDateMsg, invalidPhoneMsg)
      case x => fail(s"Unexpected $x")
    }

  }
  "WebForm.validWebForm" should "return Failure containing invalid date and phone" in {

    inside(validWebForm("valiName", "aaa", "1212")) {
      case Failure(h, t) =>
        h shouldBe invalidDateMsg
        t shouldBe Vector(invalidPhoneMsg)
      case x => fail(s"Unexpected $x")
    }

  }

  "WebForm.validWebForm" should "return Failure containing invalid phone" in {

    inside(validWebForm("valiName", "2016-12-12", "1212")) {
      case Failure(h, t) =>
        h shouldBe invalidPhoneMsg
        t shouldBe empty
      case x => fail(s"Unexpected $x")
    }

  }
  "WebForm.validWebForm" should "return Success containing valid webForm" in {

    inside(validWebForm("valiName", "2016-12-12", "1234567890")) {
      case Success(webForm) =>
        webForm.name shouldBe "valiName"
        webForm.birthdate shouldBe new SimpleDateFormat("yyyy-MM-dd").parse("2016-12-12")
        webForm.phoneNumber shouldBe "1234567890"

      case x => fail(s"Unexpected $x")
    }

  }

}
