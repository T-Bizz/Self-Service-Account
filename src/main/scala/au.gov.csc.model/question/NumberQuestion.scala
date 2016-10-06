package au.gov.csc.model.question

import scala.xml.NodeSeq

case class NumberQuestion(
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

  def isNumeric(input: String): Boolean =
    input.forall(_.isDigit)

  override def check(answer: QuestionAnswer): Boolean =
    answer.value == correctAnswer
}
