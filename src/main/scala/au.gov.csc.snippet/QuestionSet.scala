package au.gov.csc.snippet

import java.util.Date

import scala.xml.{NodeSeq,Text}

class QuestionBase(val category: String,
                   val title: NodeSeq,
                   val helpText: NodeSeq,
                   val placeHolder: String,
                   val mustBeCorrect: Boolean,
                   val order: Int) {

  def getValidationErrors(answer: String): Seq[String] = Nil

  def check(answer: Answer):Boolean = false
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
                          override val mustBeCorrect: Boolean,
                          override val order: Int,
                          correctAnswer: String)
  extends QuestionBase(category,
                       title,
                       helpText,
                       placeHolder,
                       mustBeCorrect,
                       order) {

  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if s.length < 1 => List("answer cannot be empty")
    case other => Nil
  }

  override def check(answer:Answer): Boolean =
    answer.value.toLowerCase == correctAnswer.toLowerCase
}

case class NumberQuestion(override val category: String,
                          override val title: NodeSeq,
                          override val helpText: NodeSeq,
                          override val placeHolder: String,
                          override val mustBeCorrect: Boolean,
                          override val order: Int,
                          correctAnswer: String)
  extends QuestionBase(category,
                       title,
                       helpText,
                       placeHolder,
                       mustBeCorrect,
                       order) {

  def isNumeric(input: String): Boolean =
    input.forall(_.isDigit)

  override def getValidationErrors(answer:String): Seq[String] = Nil

  override def check(answer:Answer):Boolean =
    answer.value == correctAnswer
}

case class EmailQuestion(override val category: String,
                         override val title:NodeSeq,
                         override val helpText:NodeSeq,
                         override val placeHolder:String,
                         override val mustBeCorrect: Boolean,
                         override val order:Int,
                         correctAnswer: String)
  extends QuestionBase(category,
                       title,
                       helpText,
                       placeHolder,
                       mustBeCorrect,
                       order) {

  override def getValidationErrors(answer:String): Seq[String] = Nil

  override def check(answer:Answer):Boolean =
    answer.value.toLowerCase == correctAnswer.toLowerCase
}

object TokenGenerator {
  import net.liftweb.util.Helpers._
  def generateToken:String = {
    /* nextFuncName */
    "012345"
  }
}

case class TokenQuestion(override val category: String,
                         override val title:NodeSeq,
                         override val helpText:NodeSeq,
                         override val placeHolder:String,
                         override val mustBeCorrect: Boolean,
                         override val order:Int,
                         val target:Either[EmailAddress,PhoneNumber])
  extends QuestionBase(category,
                       title,
                       helpText,
                       placeHolder,
                       mustBeCorrect,
                       order) {

  val correctAnswer = TokenGenerator.generateToken

  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if s.length < 1 => List("answer cannot be empty")
    case other => Nil
  }

  override def check(answer:Answer):Boolean =
    answer.value.toLowerCase == correctAnswer.toLowerCase
}

case class DateQuestion(override val category: String,
                        override val title: NodeSeq,
                        override val helpText: NodeSeq,
                        override val placeHolder: String,
                        override val mustBeCorrect: Boolean,
                        override val order: Int,
                        correctAnswer: Date)
  extends QuestionBase(category,
                       title,
                       helpText,
                       placeHolder,
                       mustBeCorrect,
                       order) {

  val dateFormat = new java.text.SimpleDateFormat("YYYY-mm-DD")

  override def getValidationErrors(answer:String):Seq[String] = Nil

  override def check(answer:Answer):Boolean = try {
    dateFormat.parse(answer.value).getTime() == correctAnswer.getTime()
  } catch {
    case e:Exception => false
  }
}