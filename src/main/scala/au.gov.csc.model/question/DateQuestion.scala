package au.gov.csc.model.question

import java.util.Date
import scala.xml.NodeSeq

case class DateQuestion(
  override val category: QuestionSetType.Value,
  override val title: NodeSeq,
  override val helpText: NodeSeq,
  override val placeHolder: String,
  override val icon: String,
  override val mustBeCorrect: Boolean,
  override val order: Int,
  val correctAnswer: Date
)
    extends QuestionBase(
      category,
      title,
      helpText,
      placeHolder,
      icon,
      mustBeCorrect,
      order
    ) {

  val dateFormat = new java.text.SimpleDateFormat("YYYY-mm-DD")

  override def getValidationErrors(answer: String): Seq[String] = Nil

  override def check(answer: QuestionAnswer): Boolean = {
    try {
      dateFormat.parse(answer.value).getTime() == correctAnswer.getTime()
    } catch {
      case e: Exception => false
    }
  }
}
