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

  override def check(answer:Answer):Boolean =
    answer.value.toLowerCase == correctAnswer.toLowerCase
}

object TokenGenerator extends TokenGenerator                       
class TokenGenerator {
  import net.liftweb.util.Helpers._
  def generateToken:String = {
    //nextFuncName
    "012345"
  }
}

object TokenQuestion {
  def apply(category: QuestionSetType.Value,
            title:NodeSeq,
            helpText:NodeSeq,
            placeHolder:String,
            icon: String,
            mustBeCorrect: Boolean,
            order:Int,
            target:Either[EmailAddress,PhoneNumber]) = {
    new TokenQuestion(category,title,helpText,placeHolder,icon,mustBeCorrect,order,target)
  }
  def unapply(input:TokenQuestion):Option[Tuple8[QuestionSetType.Value,NodeSeq,NodeSeq,String,String,Boolean,Int,Either[EmailAddress,PhoneNumber]]] = {
    Some(input.category,input.title,input.helpText,input.placeHolder,input.icon,input.mustBeCorrect,input.order,input.target)
  }
}

class TokenQuestion(override val category: QuestionSetType.Value,
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
  protected val tokenGenerator:TokenGenerator = TokenGenerator
  override def ask:Unit = {
    var ca = tokenGenerator.generateToken
    correctAnswer = Some(ca)
    tokenSender.send(target,ca)
  }

  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if s.length < 1 => List("answer cannot be empty")
    case other => Nil
  }

  override def check(answer:Answer):Boolean =
    correctAnswer.exists(_.toLowerCase == answer.value.toLowerCase)
  def copy(category: QuestionSetType.Value = this.category,
           title:NodeSeq = this.title,
           helpText:NodeSeq = this.helpText,
           placeHolder:String = this.placeHolder,
           icon: String = this.icon,
           mustBeCorrect: Boolean = this.mustBeCorrect,
           order:Int = this.order,
           target:Either[EmailAddress,PhoneNumber] = this.target) = {
    val ts = tokenSender
    val tg = tokenGenerator
    new TokenQuestion(category,title,helpText,placeHolder,icon,mustBeCorrect,order,target){
      override protected val tokenSender = ts
      override protected val tokenGenerator = tg
    }
  }
  override def equals(other:Any):Boolean = {
    other match {
      case TokenQuestion(c,ti,h,p,i,m,o,ta) if c == category && ti == title && h == helpText && p == placeHolder && i == icon && m == mustBeCorrect && o == order && ta == target => true
      case _ => false
    }
  }
  override def hashCode:Int = {
    List(category.hashCode,title.hashCode,helpText.hashCode,placeHolder.hashCode,icon.hashCode,mustBeCorrect.hashCode,order.hashCode,target.hashCode).sum / 8
  }
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
