package au.gov.csc.snippet

import net.liftweb.http.{SessionVar, Templates}
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds._
import au.gov.csc._
import net.liftweb.http.js.JE.{JsRaw}
import net.liftweb.util.CssSel
import scala.xml._
import StageTypeChoice._

object serviceNumber extends SessionVar[Option[String]](None)
object currentFactSet extends SessionVar[Option[FactSet]](None)
object currentAccountDetails extends SessionVar[Option[AccountDefinition]](None)
object currentStage extends SessionVar[Option[StageTypeChoice]](None)

object Scheme extends RequestVar[Option[Tuple3[String,String,String]]](None)

trait DetectScheme {
  def getScheme:Option[Tuple3[String,String,String]] = {
    Scheme.is.map(a => Some(a)).getOrElse({
      S.param("scheme").map(s => {
        s.toUpperCase match {
          case "CSS" => Scheme(Some(Tuple3("CSS", "https://css.gov.au/", "/img/site_logo_css.png")))
          case "PSS" => Scheme(Some(Tuple3("PSS", "https://pss.gov.au/", "/img/site_logo_pss.png")))
          case "MSBS" => Scheme(Some(Tuple3("MSBS", "https://militarysuper.gov.au/", "/img/site_logo_msbs.png")))
          case "DFRB" => Scheme(Some(Tuple3("DFRDB", "https://dfrdb.gov.au/", "/img/site_logo_dfrdb.png")))
          case "DFRDB" => Scheme(Some(Tuple3("DFRDB", "https://dfrdb.gov.au/", "/img/site_logo_dfrdb.png")))
          case "ADFC" => Scheme(Some(Tuple3("ADFC", "https://adfsuper.gov.au/adf-cover/", "/img/site_logo_adfc.png")))
          case "1922" => Scheme(Some(Tuple3("CSS", "https://css.gov.au/", "/img/site_logo_css.png")))
          case "PNG" => Scheme(Some(Tuple3("CSC", "https://css.gov.au/", "/img/site_logo_css.png")))
          case _ => Scheme(None)
        }
      }).getOrElse(Scheme(None))
    })
  }
}

class schemeBranding extends Logger with DetectScheme {
  def render = {
    getScheme.map(s => {
      ".schemeName *" #> Text(s._1) &
        "body [class+]" #> Text("scheme%s".format(s._1.toUpperCase)) &
        ".scheme-site-link [href]" #> Text(s._2) &
        ".scheme-site-logo [src]" #> Text(s._3)
    }).getOrElse({
      S.redirectTo("/noSchemeProvided")
    })
  }
}

class singlePageApp extends Logger with DetectScheme {

  protected def ?(key: String): String = {
    // get configured string for scheme or use the default configured string
    var out = S ? "%s%s".format(key, Scheme.is.map(s => "-%s".format(s._1)).getOrElse(""))
    if (out == "%s%s".format(key, Scheme.is.map(s => "-%s".format(s._1)).getOrElse("")))
      out = S ? key
    out
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

  def showError(errorMessage: String): NodeSeq = {
    Templates(List("ajax-templates-hidden", "Error")).map(t => {
      (".error-text *" #> Text(errorMessage) &
        startOver
      ).apply(t)
    }).openOr(NodeSeq.Empty)
  }

  def showModalError(errorTitle: String, errorMessage: String): JsCmd = {
    JsRaw("jQuery('.modal-error .modal-title-text').html('%s'); jQuery('.modal-error .modal-text').html('%s'); jQuery('.modal-error').modal('show');".format(errorTitle, errorMessage))
  }

  def askForMemberNumber: NodeSeq = Templates(List("ajax-templates-hidden","AskForMemberNumber")).map(t => {
    currentStage(Some(Identify))
    (".header-title *" #> ?("identify-header") &
      ".footer-title *" #> ?("identify-footer") &
      "#serviceNumber" #> ajaxText(serviceNumber.is.getOrElse(""), s => {
      serviceNumber(Some(s))
      val mn: MembershipNumber = new MshpNumber(s)
      addValidationMarkup("form-group-serviceNumber", mn.isValid, mn.validate.headOption.getOrElse(""), "Membership Number ")
    }) &
    ".btn-submit [onclick]" #> ajaxCall(JsRaw("this"),(_s:String) => {
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
              showModalError(?("error-title"), ?(e.getMessage))
            }
          }
          case false => showModalError(?("error-title-invalid-data"), ?("invalid-nembership-number-provided"))
        }
      }).getOrElse(showModalError(?("error-title-missing-data"), ?("no-membership-number-provided")))
    })
    ).apply(t)
  }).openOr(NodeSeq.Empty)

  def obfuscatePhoneNumber(in:String):String = {
    if (in == "unknown")
      in
    else if (in.length > 2) {
      in.substring(in.length - 2)
    }
    else
      in
  }

  def obfuscateEmailAddress(in:String):String = {
    if (in == "unknown")
      in
    else if (in.contains("@"))
      in.split("@").toList.mkString("")
    else
      in
  }

  def provideVerificationMethodChoice(factSet:FactSet): NodeSeq = {
    currentStage(Some(Verify))
    (for {
      template <- Templates(List("ajax-templates-hidden", "provideVerificationMethodChoice"))
      memberNumber <- serviceNumber.is
    } yield {
      var choices = factSet.getChoices
      var currentChoice:Option[WorkflowTypeChoice.Value] = None
      (".header-title *" #> ?("verification-method-choice-header") &
        ".sub-header-title *" #> ?("verification-method-choice-sub-header") &
        ".footer-title *" #> ?("verification-method-choice-footer") &
        "#btn-phone" #> {(n:NodeSeq) => {
          if (choices.contains(WorkflowTypeChoice.SmsAndQuestions)){
            val mobileNumber = obfuscatePhoneNumber(factSet.getCurrentMobileNumber)
            (
              ".btn-phone-value *" #> mobileNumber &
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
            val emailAddress = obfuscateEmailAddress(factSet.getCurrentEmail)
            (
              ".btn-email-value *" #> emailAddress &
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
        ".btn-submit [onclick]" #> ajaxCall(JsRaw("this"),(_s:String) => {
          currentChoice.map(choice => {
            factSet.setChoice(choice)
            SetHtml(contentAreaId, generateCurrentPageNodeSeq)
          }).getOrElse({
            showModalError(?("error-title"), ?("no-verification-method-chosen"))
          })
        }) &
        startOver
      ).apply(template)
    }).openOr(NodeSeq.Empty)
  }

  def provideAccountDetails: NodeSeq = {
    currentStage(Some(Result))
    (for {
      template <- Templates(List("ajax-templates-hidden", "provideAccountNumber"))
      memberNumber <- serviceNumber.is
    } yield {
      factProvider.getAccount(memberNumber) match {
        case Right(accountDefinition) => {
          (".header-title *" #> ?("result-header") &
              ".footer-title *" #> ?("result-footer") &
              ".membership-number *" #> accountDefinition.password &
              ".password *" #> accountDefinition.memberNumber &
              ".scheme-value *" #> accountDefinition.scheme &
              startOver
            ).apply(template)
        }
        case Left(e) => {
          showError(e.getMessage)
        }
      }
    }).openOr(NodeSeq.Empty)
  }

  def challengeFactSet(factSet:FactSet): NodeSeq = {
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
                  case other => showModalError(?("error-title"), other.mkString)
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
                    ".question-input [title]" #> question.title &
                    ".question-input [onchange]" #> ajaxCall(JsRaw("this.value"), answerQuestionFunc) &
                    ".question-input [placeholder]" #> question.placeHolder &
                    ".question-help-text [data-content]" #> question.helpText &
                    ".question-help-text .sr-only *" #> question.helpText
                  ).apply(qt))
              }).openOr(NodeSeq.Empty)
            }) &
            ".question-set-heading-description *" #> Text(?("question-set-heading-description")) &
            ".question-set-heading-contact-cic *" #> Text(?("question-set-heading-contact-cic")) &
            ".btn-submit [onclick]" #> ajaxCall(JsRaw("this"),(s:String) => {
              factSet.answerQuestions(potentialAnswers)
              SetHtml(contentAreaId, generateCurrentPageNodeSeq)
            }) &
            startOver
          ).apply(qst)
        }).openOr(NodeSeq.Empty)
      }
      case None => {
        showError(?("call-cic"))
      }
    }
  }

  protected def generateCurrentPageNodeSeq: NodeSeq = {
    val node = currentFactSet.is match {
      case None => askForMemberNumber
      case Some(factSet) if !factSet.getHasChosen && factSet.getChoices.toList.length > 1 => {
        provideVerificationMethodChoice(factSet)
      }
      case Some(factSet) if !factSet.canComplete => showError(?("call-cic"))
      case Some(factSet) if factSet.isComplete => provideAccountDetails
      case Some(factSet)                       => challengeFactSet(factSet)
    }
    (".btn-get-started-text *" #> ?("btn-get-started-text") &
      ".btn-reset-text *" #> ?("btn-reset-text") &
      ".btn-next-text *" #> ?("btn-next-text") &
      ".btn-login-text *" #> ?("btn-login-text")
    ).apply(node)
  } ++ Script(setCurrentStage)

  def startOver: CssSel = {
    ".btn-reset [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
      S.session.foreach(s => {
        s.destroySession()
        s.httpSession.foreach(httpsession => {
          httpsession.terminate
        })
      })
      RedirectTo("/?scheme=%s".format(getScheme.map(p => p._1).getOrElse("")))
    })
  }

  def render = {
    "#%s *".format(contentAreaId) #> {generateCurrentPageNodeSeq}
  }
}
// here's a mechanim for putting a function into the javascript DOM.
// Script(JsCrVar("fireError",AnonFunc(ajaxCall(JsRaw("this"),(s:String) => Noop))))