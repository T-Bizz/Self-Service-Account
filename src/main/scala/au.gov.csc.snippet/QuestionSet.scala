package au.gov.csc.snippet

import java.util.Date

import scala.xml.{NodeSeq,Text}

class QuestionBase(val category: String,
                   val title: NodeSeq,
                   val helpText: NodeSeq,
                   val placeHolder: String,
                   val order: Int) {

  def getValidationErrors(answer: String): Seq[String] =
    Nil

  def check(answer: Answer):Boolean =
    false
}

case class Answer(value: String,
                  question: QuestionBase)

case class QuestionSet(category: String,
                       title: NodeSeq,
                       questions: Seq[QuestionBase],
                       order: Int,
                       footer: Option[NodeSeq])

case class StringQuestion(override val category: String,
                          override val title: NodeSeq,
                          override val helpText: NodeSeq,
                          override val placeHolder: String,
                          override val order: Int,
                          correctAnswer: String)
  extends QuestionBase(category, title, helpText, placeHolder, order) {

  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if s.length < 1 => List("answer cannot be empty")
    case other => Nil
  }

  override def check(answer:Answer): Boolean =
    answer.value == correctAnswer
}

case class NumberQuestion(override val category: String,
                          override val title: NodeSeq,
                          override val helpText: NodeSeq,
                          override val placeHolder: String,
                          override val order: Int,
                          correctAnswer: String)
  extends QuestionBase(category, title, helpText, placeHolder, order) {

  def isNumeric(input: String): Boolean =
    input.forall(_.isDigit)

  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if !isNumeric(s) => List("answer must be numeric")
    case other => Nil
  }

  override def check(answer:Answer):Boolean =
    answer.value == correctAnswer
}

case class EmailQuestion(override val category: String,
                         override val title:NodeSeq,
                         override val helpText:NodeSeq,
                         override val placeHolder:String,
                         override val order:Int,
                         correctAnswer: String)
  extends QuestionBase(category, title, helpText, placeHolder, order) {

  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if s.length < 1 => List("answer cannot be empty")
    case other => Nil
  }

  override def check(answer:Answer):Boolean =
    answer.value == correctAnswer
}

object TokenGenerator {
  import net.liftweb.util.Helpers._
  def generateToken:String = nextFuncName
}
case class TokenQuestion(override val category: String,
                         override val title:NodeSeq,
                         override val helpText:NodeSeq,
                         override val placeHolder:String,
                         override val order:Int,
                         val target:Either[EmailAddress,PhoneNumber])
  extends QuestionBase(category, title, helpText, placeHolder, order) {
  val correctAnswer = TokenGenerator.generateToken
  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if s.length < 1 => List("answer cannot be empty")
    case other => Nil
  }

  override def check(answer:Answer):Boolean =
    answer.value == correctAnswer
}

case class DateQuestion(override val category: String,
                        override val title: NodeSeq,
                        override val helpText: NodeSeq,
                        override val placeHolder: String,
                        override val order: Int,
                        correctAnswer: Date)
  extends QuestionBase(category, title, helpText, placeHolder, order){

  val dateFormat = new java.text.SimpleDateFormat("YYYY-mm-DD")

  override def getValidationErrors(answer:String):Seq[String] = try {
    dateFormat.parse(answer)
    Nil
  } catch {
    case e:Exception => List("could not interpret answer as date")
  }

  override def check(answer:Answer):Boolean = try {
    dateFormat.parse(answer.value).getTime() == correctAnswer.getTime()
  } catch {
    case e:Exception => false
  }
}

case class IntQuestion(override val category: String,
                       override val title: NodeSeq,
                       override val helpText: NodeSeq,
                       override val placeHolder: String,
                       override val order: Int,
                       correctAnswer: Int)
  extends QuestionBase(category, title, helpText, placeHolder, order) {

  override def getValidationErrors(answer:String): Seq[String] = try {
    answer.toInt
    Nil
  } catch {
    case e:Exception => List("could not interpret answer as integer")
  }

  override def check(answer:Answer):Boolean = try {
    answer.value.toInt == correctAnswer
  } catch {
    case e:Exception => false
  }
}

case class DoubleQuestion(override val category: String,
                          override val title: NodeSeq,
                          override val helpText: NodeSeq,
                          override val placeHolder: String,
                          override val order: Int,
                          correctAnswer: Double)
  extends QuestionBase(category, title, helpText, placeHolder, order){

  var threshold = 0.01

  def withinThreshold(a:Double,b:Double): Boolean = {
    a - threshold > b || a + threshold < b
  }

  override def getValidationErrors(answer:String): Seq[String] = try {
    answer.toDouble
    Nil
  } catch {
    case e:Exception => List("could not interpret answer as double")
  }

  override def check(answer:Answer): Boolean = try {
    withinThreshold(answer.value.toDouble,correctAnswer)
  } catch {
    case e:Exception => false
  }
}



