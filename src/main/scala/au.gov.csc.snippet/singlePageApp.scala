package au.gov.csc.snippet

import net.liftweb.http.{SessionVar, Templates}
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds._
import au.gov.csc._
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.util.CssSel
import scala.xml._
import StageTypeChoice._

object serviceNumber extends SessionVar[Option[String]](None)
object currentFactSet extends SessionVar[Option[FactSet]](None)
object currentAccountDetails extends SessionVar[Option[AccountDefinition]](None)
object currentStage extends SessionVar[Option[StageTypeChoice]](None)

class singlePageApp extends Logger {

  val contentAreaId = "step-form"
  protected var factProvider = SessionState.userProvider

  def getCurrentStageJs(s:String): String = {
    "setStage($, '%s');".format(s)
  }

  def setCurrentStage: JsCmd = currentStage.is match {
    case Some(StageTypeChoice.Verify) => JsCmds.Run(getCurrentStageJs("2"))
    case Some(StageTypeChoice.Result) => JsCmds.Run(getCurrentStageJs("3"))
    case None | Some(_) | Some(StageTypeChoice.Identify) => JsCmds.Run(getCurrentStageJs("1"))
  }

  def addValidationMarkup(formGroupId: String, isValid: Boolean, error: String, errorPrefix: String): JsCmd = {
    if (isValid) {
      JsCmds.Run("jQuery('#%s').removeClass('has-error').addClass('has-success');".format(formGroupId) +
        "jQuery('#%s').find('.help-block').remove();".format(formGroupId))
    } else {
      JsCmds.Run("jQuery('#%s').removeClass('has-success').addClass('has-error');".format(formGroupId) +
        "jQuery('#%s').find('.help-block').remove();".format(formGroupId) +
        "jQuery('#%s .input-group').after('<span class=\"help-block\">%s</span>');".format(formGroupId, errorPrefix + error))
    }
  }

  def askForMemberNumber = Templates(List("ajax-templates-hidden","AskForMemberNumber")).map(t => {
    currentStage(Some(Identify))
    ("#serviceNumber" #> ajaxText(serviceNumber.is.getOrElse(""), s => {
      serviceNumber(Some(s))
      val mn: MembershipNumber = new MshpNumber(s)
      addValidationMarkup("form-group-serviceNumber", mn.isValid, mn.validate.headOption.getOrElse(""), "Membership Number ")
    }) &
    ".submitButton [onclick]" #> ajaxCall(JsRaw("this"),(_s:String) => {
      serviceNumber.is.map(s => {
        new MshpNumber(serviceNumber.is.getOrElse("")).isValid match {
          case true => factProvider.getFacts(s) match {
            case Right(member) => {
              currentFactSet(Some(new MemberBackedFactSet(member, SessionState.minimumCorrectAnswers, SessionState.pageSize)))
              SetHtml(contentAreaId, generateCurrentPageNodeSeq)
            }
            case Left(e) => {
              Alert(e.getMessage)
            }
          }
          case false => Alert("please provide a valid member number")
        }
      }).getOrElse(Alert("please provide a member number"))
    })
    ).apply(t)
  }).openOr(NodeSeq.Empty)

  def provideVerificationMethodChoice = {
    currentStage(Some(Verify))
    (for {
      template <- Templates(List("ajax-templates-hidden", "provideVerificationMethodChoice"))
      memberNumber <- serviceNumber.is
    } yield {
      (".header-title *" #> Templates(List("ajax-text-snippets-hidden", "route-0-step-1-header")) &
        ".footer-title *" #> Templates(List("ajax-text-snippets-hidden", "route-0-step-1-footer")) &
        ".submitButton [onclick]" #> ajaxCall(JsRaw("this"),(_s:String) => {
          SetHtml(contentAreaId, generateCurrentPageNodeSeq)
        }) &
        startOver
      ).apply(template)
    }).openOr(NodeSeq.Empty)
  }

  def provideAccountDetails = {
    currentStage(Some(Result))
    (for {
      template <- Templates(List("ajax-templates-hidden", "provideAccountNumber"))
      memberNumber <- serviceNumber.is
    } yield {
      factProvider.getAccount(memberNumber) match {
        case Right(accountDefinition) => {
          (
            ".accessNumberValue [value]" #> accountDefinition.password &
              ".serviceNumberValue [value]" #> accountDefinition.memberNumber &
              ".schemeValue [value]" #> accountDefinition.scheme &
              startOver
            ).apply(template)
        }
        case Left(e) => {
          Text(e.getMessage)
        }
      }
    }).openOr(NodeSeq.Empty)
  }

  def challengeFactSet(factSet:FactSet) = {
    currentStage(Some(Verify))
    factSet.getNextQuestions match {
      case Some(questionSet) => {
        var potentialAnswers:List[Answer] = Nil
        Templates(List("ajax-templates-hidden","QuestionSet")).map(qst => {(
          ".question-set-header *" #> questionSet.title &
            ".question-set-footer *" #> questionSet.footer &
            ".questions *" #> questionSet.questions.toList.foldLeft(NodeSeq.Empty)((acc, question) => {

              val answerQuestionFunc = (answerString: String) => {
                question.getValidationErrors(answerString) match {
                  case Nil => {
                    potentialAnswers = Answer(answerString, question) :: potentialAnswers
                    Noop
                  }
                  case other => Alert(other.mkString)
                }
              }

              val questionTemplate = question match {
                case d: DateQuestion => Templates(List("ajax-templates-hidden", "DateQuestion"))
                case s: StringQuestion => Templates(List("ajax-templates-hidden", "StringQuestion"))
                case n: NumberQuestion => Templates(List("ajax-templates-hidden", "NumberQuestion"))
                case e: EmailQuestion => Templates(List("ajax-templates-hidden", "EmailQuestion"))
              }

              acc ++ questionTemplate.map(qt => {
                ((
                  ".question-title *" #> question.title &
                    ".question-input [onchange]" #> ajaxCall(JsRaw("this.value"), answerQuestionFunc) &
                    ".question-input [placeholder]" #> question.placeHolder &
                    ".question-help-text [data-content]" #> question.helpText
                  ).apply(qt))
              }).openOr(NodeSeq.Empty)
            }) &
            ".submitButton [onclick]" #> ajaxCall(JsRaw("this"),(s:String) => {
              factSet.answerQuestions(potentialAnswers)
              SetHtml(contentAreaId, generateCurrentPageNodeSeq)
            }) &
            startOver
          ).apply(qst)
        }).openOr(NodeSeq.Empty)
      }
      case None => {
        Text("We couldn't verify your identity. Contact our Customer Information Center (CIC).")
      }
    }
  }

  protected def generateCurrentPageNodeSeq: NodeSeq = {
    currentFactSet.is match {
      case None => askForMemberNumber
      case Some(factSet) => provideVerificationMethodChoice
      case Some(factSet) if !factSet.canComplete => Text("We can't verify your identity. Contact our Customer Information Center (CIC).")
      case Some(factSet) if factSet.isComplete => provideAccountDetails
      case Some(factSet)                       => challengeFactSet(factSet)
    }
  } ++ Script(setCurrentStage)

  def startOver: CssSel = {
    "#reset [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
      S.session.foreach(s => {
        s.destroySession()
        s.httpSession.foreach(httpsession => {
          httpsession.terminate
        })
      })
      SetHtml(contentAreaId, generateCurrentPageNodeSeq)
    })
  }

  def render = {
    "#%s *".format(contentAreaId) #> generateCurrentPageNodeSeq
  }
}