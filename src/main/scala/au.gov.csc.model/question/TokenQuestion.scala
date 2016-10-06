package au.gov.csc.model.question

import scala.xml.NodeSeq
import au.gov.csc.model._
import au.gov.csc.model.member._
import au.gov.csc.model.fact._
import au.gov.csc.model.state._

class TokenQuestion(
  override val category: QuestionSetType.Value,
  override val title: NodeSeq,
  override val helpText: NodeSeq,
  override val placeHolder: String,
  override val icon: String,
  override val mustBeCorrect: Boolean,
  override val order: Int,
  val target: Either[EmailAddress, PhoneNumber]
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

  var correctAnswer: Option[String] = None
  protected val tokenSender = Globals.tokenSender
  protected val tokenGenerator: TokenGenerator = Globals.tokenGenerator

  override def ask(fs: FactSet): Unit = {
    var ca = tokenGenerator.generateToken
    correctAnswer = Some(ca)
    tokenSender.send(target, ca, fs)
  }

  override def getValidationErrors(answer: String): Seq[String] = answer match {
    case s if s.length < 1 => List("answer cannot be empty")
    case other             => Nil
  }

  override def check(answer: QuestionAnswer): Boolean = {
    correctAnswer.exists(_.toLowerCase == answer.value.toLowerCase)
  }

  def copy(
    category: QuestionSetType.Value = this.category,
    title: NodeSeq = this.title,
    helpText: NodeSeq = this.helpText,
    placeHolder: String = this.placeHolder,
    icon: String = this.icon,
    mustBeCorrect: Boolean = this.mustBeCorrect,
    order: Int = this.order,
    target: Either[EmailAddress, PhoneNumber] = this.target
  ) = {
    val ts = tokenSender
    val tg = tokenGenerator
    new TokenQuestion(category, title, helpText, placeHolder, icon, mustBeCorrect, order, target) {
      override protected val tokenSender = ts
      override protected val tokenGenerator = tg
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case TokenQuestion(c, ti, h, p, i, m, o, ta) if c == category && ti == title && h == helpText && p == placeHolder && i == icon && m == mustBeCorrect && o == order && ta == target => true
      case _ => false
    }
  }

  override def hashCode: Int = {
    List(category.hashCode, title.hashCode, helpText.hashCode, placeHolder.hashCode, icon.hashCode, mustBeCorrect.hashCode, order.hashCode, target.hashCode).sum / 8
  }
}

object TokenQuestion {
  def apply(
    category: QuestionSetType.Value,
    title: NodeSeq,
    helpText: NodeSeq,
    placeHolder: String,
    icon: String,
    mustBeCorrect: Boolean,
    order: Int,
    target: Either[EmailAddress, PhoneNumber]
  ) = {
    new TokenQuestion(category, title, helpText, placeHolder, icon, mustBeCorrect, order, target)
  }
  def unapply(input: TokenQuestion): Option[Tuple8[QuestionSetType.Value, NodeSeq, NodeSeq, String, String, Boolean, Int, Either[EmailAddress, PhoneNumber]]] = {
    Some(input.category, input.title, input.helpText, input.placeHolder, input.icon, input.mustBeCorrect, input.order, input.target)
  }
}