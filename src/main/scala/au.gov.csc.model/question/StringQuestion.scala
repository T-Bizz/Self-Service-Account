package au.gov.csc.model.question

import scala.xml.NodeSeq

case class StringQuestion(
  override val category: QuestionSetType.Value,
  override val title: NodeSeq,
  override val helpText: NodeSeq,
  override val placeHolder: String,
  override val icon: String,
  override val mustBeCorrect: Boolean,
  override val order: Int,
  correctAnswer: String
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

  override def check(answer: QuestionAnswer): Boolean =
    answer.value.toLowerCase == correctAnswer.toLowerCase
}