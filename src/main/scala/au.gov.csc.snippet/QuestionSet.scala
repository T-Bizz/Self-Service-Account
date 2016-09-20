package au.gov.csc.snippet

import java.util.Date
import scala.xml.{NodeSeq,Text}
import au.gov.csc.SessionState

case class QuestionSet(category: QuestionSetType.Value,
                       title: NodeSeq,
                       questions: Seq[QuestionBase],
                       order: Int,
                       footer: Option[NodeSeq])

class QuestionBase(val category: QuestionSetType.Value,
                   val title: NodeSeq,
                   val helpText: NodeSeq,
                   val placeHolder: String,
                   val icon: String,
                   val mustBeCorrect: Boolean,
                   val order: Int) {

  def getValidationErrors(answer: String): Seq[String] = Nil
  def ask:Unit = {}
  def check(answer: Answer):Boolean = false
}

case class Answer(value: String,
                  question: QuestionBase)

case class StringQuestion(override val category: QuestionSetType.Value,
                          override val title: NodeSeq,
                          override val helpText: NodeSeq,
                          override val placeHolder: String,
                          override val icon: String,
                          override val mustBeCorrect: Boolean,
                          override val order: Int,
                          correctAnswer: String)
  extends QuestionBase(category,
                       title,
                       helpText,
                       placeHolder,
                       icon,
                       mustBeCorrect,
                       order) {

  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if s.length < 1 => List("answer cannot be empty")
    case other => Nil
  }

  override def check(answer:Answer): Boolean =
    answer.value.toLowerCase == correctAnswer.toLowerCase
}

case class NumberQuestion(override val category: QuestionSetType.Value,
                          override val title: NodeSeq,
                          override val helpText: NodeSeq,
                          override val placeHolder: String,
                          override val icon: String,
                          override val mustBeCorrect: Boolean,
                          override val order: Int,
                          correctAnswer: String)
  extends QuestionBase(category,
                       title,
                       helpText,
                       placeHolder,
                       icon,
                       mustBeCorrect,
                       order) {

  def isNumeric(input: String): Boolean =
    input.forall(_.isDigit)

  override def getValidationErrors(answer:String): Seq[String] = Nil

  override def check(answer:Answer):Boolean =
    answer.value == correctAnswer
}

case class EmailQuestion(override val category: QuestionSetType.Value,
                         override val title:NodeSeq,
                         override val helpText:NodeSeq,
                         override val placeHolder:String,
                         override val icon: String,
                         override val mustBeCorrect: Boolean,
                         override val order:Int,
                         correctAnswer: String)
  extends QuestionBase(category,
                       title,
                       helpText,
                       placeHolder,
                       icon,
                       mustBeCorrect,
                       order) {

  override def getValidationErrors(answer:String): Seq[String] = Nil

  override def check(answer:Answer):Boolean =
    answer.value.toLowerCase == correctAnswer.toLowerCase
}

object TokenGenerator {
  import net.liftweb.util.Helpers._
  def generateToken:String = {
    nextFuncName
    //"012345"
  }
}

case class TokenQuestion(override val category: QuestionSetType.Value,
                         override val title:NodeSeq,
                         override val helpText:NodeSeq,
                         override val placeHolder:String,
                         override val icon: String,
                         override val mustBeCorrect: Boolean,
                         override val order:Int,
                         val target:Either[EmailAddress,PhoneNumber])
  extends QuestionBase(category,
                       title,
                       helpText,
                       placeHolder,
                       icon,
                       mustBeCorrect,
                       order) {

  var correctAnswer:Option[String] = None
  protected val tokenSender = SessionState.tokenSender
  override def ask:Unit = {
    var ca = TokenGenerator.generateToken
    correctAnswer = Some(ca)
    tokenSender.send(target,ca)
  }

  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if s.length < 1 => List("answer cannot be empty")
    case other => Nil
  }

  override def check(answer:Answer):Boolean =
    correctAnswer.exists(_.toLowerCase == answer.value.toLowerCase)
}

case class DateQuestion(override val category: QuestionSetType.Value,
                        override val title: NodeSeq,
                        override val helpText: NodeSeq,
                        override val placeHolder: String,
                        override val icon: String,
                        override val mustBeCorrect: Boolean,
                        override val order: Int,
                        correctAnswer: Date)
  extends QuestionBase(category,
                       title,
                       helpText,
                       placeHolder,
                       icon,
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
