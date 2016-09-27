package au.gov.csc.snippet

import au.gov.csc.model._
import au.gov.csc.comet._
import net.liftweb.http.{ SessionVar, Templates }
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.{ JsCmd, JsCmds }
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE.{ JsRaw }
import net.liftweb.util.CssSel
import scala.xml._
import StageTypeChoice._

import au.gov.csc.model.SessionState.{ serviceNumber, Scheme, currentFactSet, currentAccountDetails, currentStage }

trait SinglePageAppView extends DetectScheme with Logger {

  protected def ?(key: String): String = {
    // get configured string for scheme or use the default configured string
    var out = S ? "%s%s".format(key, Scheme.is.map(s => "-%s".format(s._1)).getOrElse(""))
    if (out == "%s%s".format(key, Scheme.is.map(s => "-%s".format(s._1)).getOrElse("")))
      out = S ? key
    out
  }

  protected val contentAreaId = "step-form"

  protected val factProvider = Globals.userProvider

  def getCurrentStageJs(s: String): String = {
    "setStage($, '%s');".format(s)
  }

  def setCurrentStage: JsCmd = currentStage.is match {
    case Some(StageTypeChoice.Verify)                    => JsCmds.Run(getCurrentStageJs("2"))
    case Some(StageTypeChoice.Result)                    => JsCmds.Run(getCurrentStageJs("3"))
    case None | Some(_) | Some(StageTypeChoice.Identify) => JsCmds.Run(getCurrentStageJs("1"))
  }

  def addValidationMarkup(formGroupId: String, isValid: Boolean, error: String, errorPrefix: String): JsCmd = {
    if (isValid) {
      JsCmds.Run("jQuery('#%s').removeClass('has-error');".format(formGroupId) +
        "jQuery('#%s').find('.help-block').remove();".format(formGroupId))
    } else {
      JsCmds.Run("jQuery('#%s').addClass('has-error');".format(formGroupId) +
        "jQuery('#%s').find('.help-block').remove();".format(formGroupId) +
        "jQuery('#%s .input-group').after('<span class=\"help-block\" aria-live=\"assertive\" aria-relevant=\"additions removals\">%s</span>');".format(formGroupId, errorPrefix + error))
    }
  }

  def showError(errorMessage: String): NodeSeq = {
    currentStage(Some(Result))
    Templates(List("ajax-templates-hidden", "Error")).map(t => {
      (".error-text *" #> Text(errorMessage) &
        startOver(".btn-restart [onclick]", "/")).apply(t)
    }).openOr(NodeSeq.Empty)
  }

  def safetyForJs(input: String): String = {
    input.replace('"', '\'')
  }

  def showModalError(title: String, message: String): JsCmd = {
    JsRaw("jQuery(\".modal-error .modal-title-text\").html(\"%s\"); jQuery(\".modal-error .modal-text\").html(\"%s\"); jQuery(\".modal-error\").modal(\"show\");".format(safetyForJs(title), safetyForJs(message)))
  }

  def showModal(title: String, message: String): JsCmd = {
    JsRaw("jQuery(\".modal-general .modal-title-text\").html(\"%s\"); jQuery(\".modal-general .modal-text\").html(\"%s\"); jQuery(\".modal-general\").modal(\"show\");".format(safetyForJs(title), safetyForJs(message)))
  }

  def focusFirstInputField: JsCmd = {
    JsRaw("jQuery(\"#%s\").find('*').filter(':input:visible:first');".format(contentAreaId))
  }

  protected val pageId = nextFuncName
  def pushUserAction(redirectPath: Option[String] = None) = {
    PushActorManager ! NavigationMessage(SessionState.sessionId.is, pageId, redirectPath)
  }

  def subUserAction(id: String, redirectPath: Option[String] = None): JsCmd = {
    if (id != pageId) {
      redirectPath match {
        case Some(p) =>
          ajaxCall(JsRaw("this"), (_s: String) => {
            RedirectTo(p)
          })
        case None =>
          ajaxCall(JsRaw("this"), (s: String) => {
            SetHtml(contentAreaId, generateCurrentPageNodeSeq)
          })
      }
    }
  }

  def askForMemberNumber: NodeSeq = Templates(List("ajax-templates-hidden", "AskForMemberNumber")).map(t => {
    currentStage(Some(Identify))
    (".header-title *" #> ?("identify-header") &
      ".footer-title *" #> ?("identify-footer") &
      ".btn-submit [onclick]" #> ajaxCall(JsRaw("jQuery('#serviceNumber').val()"), (s: String) => {
        serviceNumber(Some(s))
        val mn: MembershipNumber = new MshpNumber(s)
        if (currentFactSet.is.isDefined) {
          showModalError(?("error-title-invalid-data"), ?("membership-number-already-provided")) & SetHtml(contentAreaId, generateCurrentPageNodeSeq)
        } else {
          serviceNumber.is.map(s => {
            new MshpNumber(serviceNumber.is.getOrElse("")).isValid match {
              case true => factProvider.getFacts(s) match {
                case Right(member) => {
                  try {
                    currentFactSet(Some(new MemberBackedFactSet(member, SessionState.minimumCorrectAnswers, SessionState.pageSize)))
                  } catch {
                    case t: Throwable =>
                      error("exception: %s\r\n%s".format(t.getMessage, t.getStackTrace))
                  }
                  pushUserAction()
                  SetHtml(contentAreaId, generateCurrentPageNodeSeq)
                }
                case Left(e) => {
                  showModalError(?("error-title"), ?(e.getMessage))
                }
              }
              case false => showModalError(?("error-title-invalid-data"), ?("invalid-nembership-number-provided")) & addValidationMarkup("form-group-serviceNumber", mn.isValid, mn.validate.headOption.getOrElse(""), "Membership Number ")
            }
          }).getOrElse(showModalError(?("error-title-missing-data"), ?("no-membership-number-provided")))
        }
      })).apply(t)
  }).openOr(NodeSeq.Empty)

  private def obscure(text: String) = "*" * text.length

  def obfuscatePhoneNumber(in: String): String = {
    if (in == "unknown")
      in
    else if (in.length > 2) {
      in.substring(in.length - 2)
    } else
      in
  }

  def obfuscateEmailAddress(in: String): String = {
    val shortMailbox = "(.{1,2})".r
    val longMailbox = "(.)(.*)(.)".r
    val domain = in.split("@").toList.tail(0)

    in match {
      case shortMailbox(address) => s"${obscure(address)}@$domain"
      case longMailbox(firstLetter, middle, lastLetter) => s"$firstLetter${obscure(middle)}$lastLetter@$domain"
      case _ => in
    }
  }

  def provideVerificationMethodChoice(factSet: FactSet): NodeSeq = {
    currentStage(Some(Verify))
    (for {
      template <- Templates(List("ajax-templates-hidden", "provideVerificationMethodChoice"))
      memberNumber <- serviceNumber.is
    } yield {
      var choices = factSet.getChoices
      var currentChoice: Option[WorkflowTypeChoice.Value] = None
      (".header-title *" #> ?("verification-method-choice-header") &
        ".sub-header-title *" #> ?("verification-method-choice-sub-header") &
        ".footer-title *" #> ?("verification-method-choice-footer") &
        "#btn-phone" #> { (n: NodeSeq) =>
          {
            if (choices.contains(WorkflowTypeChoice.SmsAndQuestions)) {

              val mobileNumber = obfuscatePhoneNumber(factSet.getCurrentMobileNumber)

              (
                ".btn-phone-value *" #> mobileNumber &
                "#btn-phone [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
                  currentChoice = Some(WorkflowTypeChoice.SmsAndQuestions)
                  Noop
                })
              ).apply(n)
            } else {
              NodeSeq.Empty
            }
          }
        } &
        "#btn-email" #> { (n: NodeSeq) =>
          {
            if (choices.contains(WorkflowTypeChoice.EmailAndQuestions)) {
              val emailAddress = obfuscateEmailAddress(factSet.getCurrentEmail)
              (
                ".btn-email-value *" #> emailAddress &
                "#btn-email [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
                  currentChoice = Some(WorkflowTypeChoice.EmailAndQuestions)
                  Noop
                })
              ).apply(n)
            } else {
              NodeSeq.Empty
            }
          }
        } &
        "#btn-other" #> { (n: NodeSeq) =>
          {
            if (choices.contains(WorkflowTypeChoice.QuestionsOnly)) {
              (
                "#btn-other [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
                  currentChoice = Some(WorkflowTypeChoice.QuestionsOnly)
                  Noop
                })
              ).apply(n)
            } else {
              NodeSeq.Empty
            }
          }
        } &
        ".btn-submit [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
          if (factSet.getHasChosen) {
            showModalError(?("error-title"), ?("workflow-already-chosen")) & SetHtml(contentAreaId, generateCurrentPageNodeSeq)
          } else {
            currentChoice.map(choice => {
              factSet.setChoice(choice)
              pushUserAction()
              SetHtml(contentAreaId, generateCurrentPageNodeSeq)
            }).getOrElse({
              showModalError(?("error-title"), ?("no-verification-method-chosen"))
            })
          }
        })).apply(template)
    }).openOr(NodeSeq.Empty)
  }

  def provideAccountDetails: NodeSeq = {
    currentStage(Some(Result))
    (for {
      template <- Templates(List("ajax-templates-hidden", "provideAccountNumber"))
      memberNumber <- serviceNumber.is
      fs <- currentFactSet.is
    } yield {
      (".header-title *" #> ?("result-header") &
        ".footer-title *" #> ?("result-footer") &
        ".account-list *" #> fs.getEligibleAccountChoice.toList.foldLeft(NodeSeq.Empty)((acc, mshp) => {
          acc ++ (Templates(List("ajax-templates-hidden", "accountNumberResult")).map(t => {
            (".account-id *" #> mshp.external_id &
              ".account-scheme *" #> mshp.scheme &
              ".account-status *" #> mshp.status &
              ".account-password *" #> (factProvider.getAccount(mshp.external_id) match {
                case Right(accountDefinition) => Text(accountDefinition.password)
                case Left(e)                  => Text("Password could not be set")
              })).apply(t)
          })).openOr(NodeSeq.Empty)
        })).apply(template)
    }).openOr(NodeSeq.Empty)
  }

  def provideAccountChoice: NodeSeq = {
    currentStage(Some(Result))
    (for {
      template <- Templates(List("ajax-templates-hidden", "provideAccountChoice"))
      fs <- currentFactSet.is
    } yield {
      var currentChoice: Seq[Membership] = Seq()
        def toggleChoice(in: Membership) = {
          if (currentChoice.contains(in)) {
            currentChoice = currentChoice.filterNot(c => {
              c == in
            })
          } else {
            currentChoice = (currentChoice :+ in)
          }
        }
      (".header-title *" #> ?("account-choice-header") &
        ".sub-header-title *" #> ?("account-choice-sub-header") &
        ".footer-title *" #> ?("account-choice-footer") &
        ".account-list *" #> fs.getEligibleMemberships.toList.foldLeft(NodeSeq.Empty)((acc, mshp) => {

          acc ++ (Templates(List("ajax-templates-hidden", "accountChoiceButton")).map(t => {
            (".account-id *" #> mshp.external_id &
              ".account-scheme *" #> mshp.scheme &
              ".account-status *" #> mshp.status &
              ".list-group-item [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
                toggleChoice(mshp)
                Noop
              })).apply(t)
          })).openOr(NodeSeq.Empty)
        }) &
        ".btn-submit [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
          if (currentChoice.size >= 1) {
            fs.setEligibleAccountChoice(currentChoice)
            pushUserAction()
            SetHtml(contentAreaId, generateCurrentPageNodeSeq)
          } else {
            showModalError(?("error-title-missing-data"), ?("no-account-chosen"))
          }
        })).apply(template)
    }).openOr(NodeSeq.Empty)
  }

  def challengeFactSet(factSet: FactSet): NodeSeq = {
    currentStage(Some(Verify))
    factSet.getNextQuestions match {
      case Some(questionSet) => {
        var potentialAnswers: List[Answer] = Nil
        Templates(List("ajax-templates-hidden", "QuestionSet")).map(qst => {
          (
            ".question-set-header *" #> questionSet.title &
            ".question-set-footer *" #> questionSet.footer &
            ".questions *" #> questionSet.questions.toList.foldLeft(NodeSeq.Empty)((acc, question) => {
              // every question that has been presented on the screen has been answered, this way users can skip questions
              potentialAnswers = Answer("", question) :: potentialAnswers

              val answerQuestionFunc = (answerString: String) => {
                potentialAnswers = potentialAnswers.filterNot(pa => {
                  pa.question == question
                })
                potentialAnswers = Answer(answerString, question) :: potentialAnswers
                question.getValidationErrors(answerString) match {
                  case Nil => Noop
                  case o   => showModalError(?("error-title"), o.mkString)
                }
              }

              val askQuestionFunc = (_unused: String) => {
                question.ask(factSet)
                showModal(?("token-sent-title"), ?("token-sent-description"))
              }

              val questionTemplate = question match {
                case d: DateQuestion   => Templates(List("ajax-templates-hidden", "DateQuestion"))
                case s: StringQuestion => Templates(List("ajax-templates-hidden", "StringQuestion"))
                case n: NumberQuestion => Templates(List("ajax-templates-hidden", "NumberQuestion"))
                case e: EmailQuestion  => Templates(List("ajax-templates-hidden", "EmailQuestion"))
                case t: TokenQuestion  => Templates(List("ajax-templates-hidden", "TokenQuestion"))
                case _                 => Empty
              }

              askQuestionFunc("")
              acc ++ questionTemplate.map(qt => {
                ((
                  ".question-title *" #> question.title &
                  ".question-input [title]" #> question.title &
                  ".question-input [onchange]" #> ajaxCall(JsRaw("this.value"), answerQuestionFunc) &
                  ".question-input [placeholder]" #> question.placeHolder &
                  ".question-help-text [data-content]" #> question.helpText &
                  ".question-help-text .sr-only *" #> question.helpText &
                  ".action-group" #> {
                    ".question-ask-action [onclick]" #> ajaxCall(JsRaw("this.value"), askQuestionFunc) &
                      ".question-ask-action-label *" #> {
                        question match {
                          case t: TokenQuestion if t.target.isLeft => Text(?("resend-token-to-email"))
                          case t: TokenQuestion                    => Text(?("resend-token-to-mobile"))
                        }
                      }
                  } &
                  ".question-icon [class+]" #> question.icon
                ).apply(qt))
              }).openOr(NodeSeq.Empty)
            }) &
            (questionSet.category match {
              case QuestionSetType.TokenEmail | QuestionSetType.TokenSMS => {
                ".question-set-heading-description *" #> Text(?("question-set-token-heading-description")) &
                  ".question-set-heading-contact-cic *" #> Text(?("question-set-token-heading-contact-cic"))
              }
              case _ => {
                ".question-set-heading-description *" #> Text(?("question-set-heading-description")) &
                  ".question-set-heading-contact-cic *" #> Text(?("question-set-heading-contact-cic"))
              }
            }) &
            ".btn-submit [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
              factSet.answerQuestions(potentialAnswers)
              pushUserAction()
              SetHtml(contentAreaId, generateCurrentPageNodeSeq)
            })
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
      case Some(factSet) if !factSet.getHasChosen && factSet.getChoices.size == 1 => {
        trace("%s has only one verification method, skipping the associated choice screen.".format(serviceNumber.is.getOrElse("")))
        factSet.getChoices.map(choice => {
          factSet.setChoice(choice)
        })
        pushUserAction()
        generateCurrentPageNodeSeq
      }
      case Some(factSet) if !factSet.getHasChosen && factSet.getChoices.size == 0 => {
        warn("%s has no verification methods available.".format(serviceNumber.is.getOrElse("")))
        showError(?("call-cic"))
      }
      case Some(factSet) if !factSet.canComplete => {
        trace("%s does not have enough facts avialable to complete the process.".format(serviceNumber.is.getOrElse("")))
        showError(?("call-cic"))
      }
      case Some(factSet) if !factSet.getHasChosen => {
        trace("%s has only one verification method, skipping the associated choice screen.".format(serviceNumber.is.getOrElse("")))
        provideVerificationMethodChoice(factSet)
      }
      case Some(factSet) if factSet.isComplete && !factSet.getHasChosenEligibleAccount &&
        factSet.getEligibleMemberships.size > 1 => {

        trace("%s has more than one eligible account for registration / reset.".format(serviceNumber.is.getOrElse("")))
        provideAccountChoice
      }
      case Some(factSet) if factSet.isComplete && !factSet.getHasChosenEligibleAccount &&
        factSet.getEligibleMemberships.size == 1 => {

        trace("%s has only one eligible account for registration / reset. Skipping choice.".format(serviceNumber.is.getOrElse("")))
        factSet.setEligibleAccountChoice(factSet.getEligibleMemberships)
        pushUserAction()
        provideAccountDetails
      }
      case Some(factSet) if factSet.isComplete && factSet.getHasChosenEligibleAccount => {

        trace("%s has had eligible accounts registered / reset.".format(serviceNumber.is.getOrElse("")))
        provideAccountDetails
      }
      case Some(factSet) => {
        trace("%s is being challenged to prove their identity.".format(serviceNumber.is.getOrElse("")))
        challengeFactSet(factSet)
      }
    }
    (".btn-get-started-text *" #> ?("btn-get-started-text") &
      ".btn-reset-text *" #> ?("btn-reset-text") &
      ".btn-restart-text *" #> ?("btn-restart-text") &
      ".btn-next-text *" #> ?("btn-next-text") &
      startOver(".btn-restart [onclick]", "/") &
      startOver()).apply(node)
  } ++ Script(setCurrentStage) ++ Script(focusFirstInputField)

  def startOver(
    csssel: String = ".btn-reset [onclick]",
    redirect: String = "/scheme/%s".format(getScheme.map(p => p._1).getOrElse(""))
  ): CssSel = {
    csssel #> ajaxCall(JsRaw("this"), (_s: String) => {
      S.session.foreach(s => {
        //s.destroySession()
        s.httpSession.foreach(httpsession => {
          pushUserAction(Some("/sessionTerminated"))
          httpsession.terminate
        })
      })
      RedirectTo(redirect)
    })
  }
}

class singlePageApp extends Logger with SinglePageAppView {
  def comet = {
    val detectedScheme = detectScheme
    val oldScheme = getScheme
    detectedScheme.filterNot(s => oldScheme.exists(_ == s)).foreach(s => {
      trace("detected scheme change: %s => %s".format(oldScheme, s))
      Scheme(detectedScheme)
      pushUserAction(Some("/scheme/%s".format(s._1)))
    })

    val cometActorName = "lift:comet?type=PushActor&name=%s".format(nextFuncName)
    ".app-root-elem [data-lift]" #> cometActorName
  }

  def render = {
    "#%s [data-id]".format(contentAreaId) #> pageId &
      "#%s *".format(contentAreaId) #> { generateCurrentPageNodeSeq }
  }
}
