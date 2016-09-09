package au.gov.csc.snippet

import net.liftweb.http.{SessionVar, Templates}
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds._
import au.gov.csc._
import net.liftweb.http.js.JE.{AnonFunc, JsFunc, JsRaw}
import net.liftweb.util.CssSel

import scala.xml._
import StageTypeChoice._

object serviceNumber extends SessionVar[Option[String]](None)
object currentFactSet extends SessionVar[Option[FactSet]](None)
object currentAccountDetails extends SessionVar[Option[AccountDefinition]](None)
object currentStage extends SessionVar[Option[StageTypeChoice]](None)

object Scheme extends RequestVar[Option[String]](None)

trait DetectScheme {
  def getScheme:Option[String] = {
    Scheme.is.map(s => Some(s)).getOrElse({
      S.param("scheme").map(s => {
        Scheme(Some(s))
        s
      })
    })
  }
}
class schemeBranding extends Logger with DetectScheme {
  def render = {
    getScheme.map(s => {
      ".schemeName *" #> s
    }).getOrElse({
      S.redirectTo("/noSchemeProvided")
    })
  }
}

class singlePageApp extends Logger with DetectScheme {

  protected def ?(key: String): String = {
    S ? "%s%s".format(key, Scheme.is.map(s => "_%s".format(s)).getOrElse(""))
  }
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
              try {
                currentFactSet(Some(new MemberBackedFactSet(member, SessionState.minimumCorrectAnswers, SessionState.pageSize)))
              } catch {
                case e:Exception => println("exception: %s\r\n%s".format(e.getMessage,e.getStackTraceString))
              }
              SetHtml(contentAreaId, generateCurrentPageNodeSeq)
            }
            case Left(e) => {
              Alert(e.getMessage)
            }
          }
          case false => Alert(?("invalidMumberNumberProvided"))
        }
      }).getOrElse(Alert(?("noMemberNumberProvided")))
    })
    ).apply(t)
  }).openOr(NodeSeq.Empty)

  def obfuscatePhoneNumber(in:String):String = {
    if (in.length > 2) {
      in.substring(in.length - 2)
    } else in
  }
  def obfuscateEmailAddress(in:String):String = {
    in.split("@").drop(1).toList.mkString("")
  }
  def provideVerificationMethodChoice(factSet:FactSet) = {
    currentStage(Some(Verify))
    (for {
      template <- Templates(List("ajax-templates-hidden", "provideVerificationMethodChoice"))
      memberNumber <- serviceNumber.is
    } yield {
      var choices = factSet.getChoices
      var currentChoice:Option[WorkflowTypeChoice.Value] = None
      (".header-title *" #> Templates(List("ajax-text-snippets-hidden", "route-0-step-1-header")) &
        ".footer-title *" #> Templates(List("ajax-text-snippets-hidden", "route-0-step-1-footer")) &
        "#btn-phone" #> {(n:NodeSeq) => {
          if (choices.contains(WorkflowTypeChoice.SmsAndQuestions)){
            var mobileNumber = "" // get this from factSet
            (
              ".interpolationValue *" #> obfuscatePhoneNumber(mobileNumber) &
              "#btn-phone [onclick]" #> ajaxCall(JsRaw("this"),(s:String) => {
                currentChoice = Some(WorkflowTypeChoice.SmsAndQuestions)
                Noop
              })
              ).apply(n)
          } else {
            NodeSeq.Empty
          }
        }} &
        "#btn-email" #> {(n:NodeSeq) => {
          if (choices.contains(WorkflowTypeChoice.EmailAndQuestions)){
            var emailAddress = "" // get this from factSet
            (
              ".interpolationValue *" #> obfuscateEmailAddress(emailAddress) &
                "#btn-email [onclick]" #> ajaxCall(JsRaw("this"),(s:String) => {
                  currentChoice = Some(WorkflowTypeChoice.EmailAndQuestions)
                  Noop
                })
              ).apply(n)
          } else {
            NodeSeq.Empty
          }
        }} &
        "#btn-other" #> {(n:NodeSeq) => {
          if (choices.contains(WorkflowTypeChoice.QuestionsOnly)){
            var mobileNumber = "" // get this from factSet
            (
                "#btn-other [onclick]" #> ajaxCall(JsRaw("this"),(s:String) => {
                  currentChoice = Some(WorkflowTypeChoice.QuestionsOnly)
                  Noop
                })
              ).apply(n)
          } else {
            NodeSeq.Empty
          }
        }} &

        ".submitButton [onclick]" #> ajaxCall(JsRaw("this"),(_s:String) => {
          currentChoice.map(choice => {
            factSet.setChoice(choice)
            SetHtml(contentAreaId, generateCurrentPageNodeSeq)
          }).getOrElse({
            Alert(?("chooseVerificationMethod"))
          })
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
                case t: TokenQuestion => Templates(List("ajax-templates-hidden", "StringQuestion"))
                case _ => Empty
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
        Text(?("callCic"))
      }
    }
  }

  protected def generateCurrentPageNodeSeq: NodeSeq = {
    //S.param("host")
    //S.req.foreach(_.param("host"))
    currentFactSet.is match {
      case None => askForMemberNumber
      case Some(factSet) if !factSet.getHasChosen && factSet.getChoices.toList.length > 1 => {
        provideVerificationMethodChoice(factSet)
      }
      case Some(factSet) if !factSet.canComplete => Text(?("callCic"))
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
      RedirectTo("/index")
      //SetHtml(contentAreaId, generateCurrentPageNodeSeq)
    })
  }

  def render = {
    getScheme.map(s => {
      "#%s *".format(contentAreaId) #> {generateCurrentPageNodeSeq}
    }).getOrElse({
      "#%s *".format(contentAreaId) #> Text(?("noSchemeProvided"))
    })
  }
}
// here's a mechanim for putting a function into the javascript DOM.
// Script(JsCrVar("fireError",AnonFunc(ajaxCall(JsRaw("this"),(s:String) => Noop))))