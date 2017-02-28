package chapter_12

import java.util.Date

import scala.util.Try

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object WebForm {

  val invalidNameMsg = "Name cannot be empty"
  val invalidDateMsg: String = "Birthdate must be in the form yyyy-MM-dd"
  val invalidPhoneMsg: String = "Phone number must be 10 digits"

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure(invalidNameMsg)

  def validBirthdate(birthdate: String): Validation[String, Date] =
  Try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } getOrElse {
      Failure(invalidDateMsg)
    }
  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
  else {
      Failure(invalidPhoneMsg)
    }


  def validWebForm(name: String,
                   birthdate: String,
                   phone: String): Validation[String, WebForm] =
    Validation.validationApplicative.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(
      WebForm(_,_,_))


}