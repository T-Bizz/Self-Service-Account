package au.gov.csc.model.question

import scala.xml.{ NodeSeq }

case class QuestionSet(
  category: QuestionSetType.Value,
  title: NodeSeq,
  questions: Seq[QuestionBase],
  order: Int,
  footer: Option[NodeSeq]
)
