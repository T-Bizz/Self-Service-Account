package au.gov.csc.model.question

import java.util.Date
import scala.xml.NodeSeq
import au.gov.csc.model._
import au.gov.csc.model.fact._

class QuestionBase(
    val category: QuestionSetType.Value,
    val title: NodeSeq,
    val helpText: NodeSeq,
    val placeHolder: String,
    val icon: String,
    val mustBeCorrect: Boolean,
    val order: Int
) {

  def getValidationErrors(answer: String): Seq[String] = Nil
  def ask(factSet: FactSet): Unit = {}
  def check(answer: QuestionAnswer): Boolean = false
}
