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

  protected val contentAreaId = "step-form"
  protected val factProvider = Globals.userProvider
  protected val pageId = nextFuncName

  protected def showModalError(title: String, message: String): JsCmd = {
    JsRaw("showModalError($, \"%s\", \"%s\");".format(escapeJs(title), escapeJs(message)))
  }

  protected def showModal(title: String, message: String): JsCmd = {
    JsRaw("showModal($, \"%s\", \"%s\");".format(escapeJs(title), escapeJs(message)))
  }

  protected def focusFirstInputField: JsCmd = {
    JsRaw("focusFirstInput($, \"%s\");".format(contentAreaId))
  }

  protected def addValidationMarkup(formGroupId: String, isValid: Boolean, error: String, errorPrefix: String): JsCmd = {
    JsRaw("addValidationMarkup($, \"%s\", \"%s\", \"%s\");".format(formGroupId, isValid, errorPrefix + error))
  }

  protected def setCurrentStage(s: String): JsCmd = {
    JsRaw("setStage($, '%s');".format(s))
  }

  protected def setCurrentStage: JsCmd = currentStage.is match {
    case Some(StageTypeChoice.Verify)                    => setCurrentStage("2")
    case Some(StageTypeChoice.SetPassword)               => setCurrentStage("3")
    case Some(StageTypeChoice.Summary)                   => setCurrentStage("4")
    case None | Some(_) | Some(StageTypeChoice.Identify) => setCurrentStage("1")
  }

  protected def ?(key: String): String =
    S.?("%s%s".format(key.toLowerCase, Scheme.is.map(s => "-%s".format(s._1)).getOrElse("").toLowerCase)) match {
      case out if out == "%s%s".format(key.toLowerCase, Scheme.is.map(s => "-%s".format(s._1)).getOrElse("").toLowerCase) => {
        trace("Could not find text snippet with key %s. Replacing it with the key %s.".format(out, key))
        S.?(key)
      }
      case out => out
    }

  def pushUserAction(redirectPath: Option[String] = None) = {
    PushActorManager ! NavigationMessage(SessionState.sessionId.is, pageId, redirectPath)
  }

  def subscribeToUserAction(id: String, redirectPath: Option[String] = None): JsCmd = {
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

  protected def startOver(
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

  protected def escapeJs(input: String): String = {
    input.replace('"', '\'')
  }

  protected def obfuscatePhoneNumber(in: String): String = in match {
    case "unknown"         => in
    case i if i.length > 2 => i.substring(i.length - 2)
    case i                 => i
  }

  protected def obfuscateEmailAddress(in: String): String = {
    val shortMailbox = "(.{1,2})".r
    val longMailbox = "(.)(.*)(.)".r
    val validDomain = """^([a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*)$""".r
    val validEmail = """^([a-zA-Z0-9.!#$%&’'*+/=?^_`{|}~-]+)@([a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*)$""".r
      def obscure(text: String) = "*" * text.length

    (in, in.split("@").toList.tail(0)) match {
      case (validEmail, validDomain) => {
        (in.split("@").toList.head, in.split("@").toList.tail(0)) match {
          case (shortMailbox(all), domain) => s"${obscure(all)}@$domain"
          case (longMailbox(first, middle, last), domain) => s"$first${obscure(middle)}$last@$domain"
          case _ => "*"
        }
      }
      case _ => {
        warn("Email address %s is invalid.".format(in))
        "*"
      }
    }
  }

  protected def askForMembershipNumber: NodeSeq = Templates(List("ajax-templates-hidden", "askForMembershipNumber")).map(t => {
    currentStage(Some(Identify))
    (".header-title *" #> ?("identify-header") &
      ".footer-title *" #> ?("identify-footer") &
      ".btn-submit [onclick]" #> ajaxCall(JsRaw("jQuery('#serviceNumber').val()"), (s: String) => {
        serviceNumber(Some(s))
        if (currentFactSet.is.isDefined) {
          showModalError(?("error-title-invalid-data"), ?("membership-number-already-provided")) &
            SetHtml(contentAreaId, generateCurrentPageNodeSeq)
        } else {
          val mn: MembershipNumber = new MshpNumber(s)
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
                  showModalError(?("error-title"), ?(e.getMessage)) &
                    addValidationMarkup("form-group-serviceNumber", mn.isValid, mn.validate.headOption.getOrElse(""), "Membership Number ")
                }
              }
              case false => showModalError(?("error-title-invalid-data"), ?("invalid-nembership-number-provided")) &
                addValidationMarkup("form-group-serviceNumber", mn.isValid, mn.validate.headOption.getOrElse(""), "Membership Number ")
            }
          }).getOrElse(showModalError(?("error-title-missing-data"), ?("no-membership-number-provided")))
        }
      })).apply(t)
  }).openOr(NodeSeq.Empty)

  protected def askForVerificationMethod(factSet: FactSet): NodeSeq = {
    currentStage(Some(Verify))
    (for {
      template <- Templates(List("ajax-templates-hidden", "askForVerificationMethod"))
      memberNumber <- serviceNumber.is
    } yield {
      var currentChoice: Option[WorkflowTypeChoice.Value] = None
      (".header-title *" #> ?("verification-method-choice-header") &
        ".sub-header-title *" #> ?("verification-method-choice-sub-header") &
        ".footer-title *" #> ?("verification-method-choice-footer") &
        "#btn-phone" #> ((n: NodeSeq) => {
          if (factSet.getChoices.contains(WorkflowTypeChoice.SmsAndQuestions))
            (".btn-phone-value *" #> obfuscatePhoneNumber(factSet.getCurrentMobileNumber) &
              "#btn-phone [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
                currentChoice = Some(WorkflowTypeChoice.SmsAndQuestions)
                Noop
              })).apply(n)
          else
            NodeSeq.Empty
        }) &
        "#btn-email" #> ((n: NodeSeq) => {
          if (factSet.getChoices.contains(WorkflowTypeChoice.EmailAndQuestions))
            (".btn-email-value *" #> obfuscateEmailAddress(factSet.getCurrentEmail) &
              "#btn-email [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
                currentChoice = Some(WorkflowTypeChoice.EmailAndQuestions)
                Noop
              })).apply(n)
          else
            NodeSeq.Empty
        }) &
        "#btn-other" #> ((n: NodeSeq) => {
          if (factSet.getChoices.contains(WorkflowTypeChoice.QuestionsOnly))
            ("#btn-other [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
              currentChoice = Some(WorkflowTypeChoice.QuestionsOnly)
              Noop
            })).apply(n)
          else
            NodeSeq.Empty
        }) &
        ".btn-submit [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
          if (factSet.getHasChosen) {
            showModalError(?("error-title"), ?("workflow-already-chosen")) &
              SetHtml(contentAreaId, generateCurrentPageNodeSeq)
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

  protected def askForProofOfIdentity(factSet: FactSet): NodeSeq = {
    currentStage(Some(Verify))
    factSet.getNextQuestions match {
      case Some(questionSet) => {
        var potentialAnswers: List[Answer] = Nil
        Templates(List("ajax-templates-hidden", "askForProofOfIdentity")).map(qst => {
          (".question-set-header *" #> questionSet.title &
            ".question-set-footer *" #> questionSet.footer &
            ".questions *" #> questionSet.questions.toList.foldLeft(NodeSeq.Empty)((acc, question) => {
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
                case d: DateQuestion   => Templates(List("ajax-templates-hidden", "questionDate"))
                case s: StringQuestion => Templates(List("ajax-templates-hidden", "questionString"))
                case n: NumberQuestion => Templates(List("ajax-templates-hidden", "questionNumber"))
                case e: EmailQuestion  => Templates(List("ajax-templates-hidden", "questionEmail"))
                case t: TokenQuestion  => Templates(List("ajax-templates-hidden", "questionToken"))
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
            })).apply(qst)
        }).openOr(NodeSeq.Empty)
      }
      case None => {
        provideError(?("call-cic"))
      }
    }
  }

  protected def askForAccounts: NodeSeq = {
    currentStage(Some(SetPassword))
    (for {
      template <- Templates(List("ajax-templates-hidden", "askForAccounts"))
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

  protected def askForPassword: NodeSeq = {
    currentStage(Some(SetPassword))
    (for {
      template <- Templates(List("ajax-templates-hidden", "askForPassword"))
      fs <- currentFactSet.is
    } yield {
      var password: String = ""
      var passwordConfirmation: String = ""
      (".header-title *" #> ?("ask-for-password-header") &
        ".footer-title *" #> ?("ask-for-password-footer") &
        ".questions *" #> (
          (Templates(List("ajax-templates-hidden", "questionPassword")).map(t => {
            (".question-title *" #> ?("password-question") &
              ".question-input [title]" #> ?("password-question") &
              ".question-input [placeholder]" #> ?("password-placeholder") &
              ".question-help-text [data-content]" #> ?("password-help-text") &
              ".question-help-text .sr-only *" #> ?("password-help-text") &
              ".question-icon [class+]" #> ?("password-icon") &
              ".question-input [onchange]" #> ajaxCall(JsRaw("this.value"), (s: String) => {
                password = s
                Noop
              })).apply(t)
          })).openOr(NodeSeq.Empty) ++
          (Templates(List("ajax-templates-hidden", "questionPassword")).map(t => {
            (".question-title *" #> ?("password-confirmation-question") &
              ".question-input [title]" #> ?("password-confirmation-question") &
              ".question-input [placeholder]" #> ?("password-confirmation-placeholder") &
              ".question-help-text [data-content]" #> ?("password-confirmation-help-text") &
              ".question-help-text .sr-only *" #> ?("password-confirmation-help-text") &
              ".question-icon [class+]" #> ?("password-confirmation-icon") &
              ".question-input [onchange]" #> ajaxCall(JsRaw("this.value"), (s: String) => {
                passwordConfirmation = s
                Noop
              })).apply(t)
          })).openOr(NodeSeq.Empty)
        ) &
          ".btn-submit [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
            if (password == passwordConfirmation) {
              SessionState.userPassword(Some(password))
              pushUserAction()
              SetHtml(contentAreaId, generateCurrentPageNodeSeq)
            } else {
              showModalError(?("error-title-invalid-data"), ?("passwords-do-not-match"))
            }
          })).apply(template)
    }).openOr(NodeSeq.Empty)
  }

  protected def providePasswords: NodeSeq = {
    currentStage(Some(Summary))
    (for {
      template <- Templates(List("ajax-templates-hidden", "providePasswords"))
      memberNumber <- serviceNumber.is
      fs <- currentFactSet.is
      pw <- SessionState.userPassword.is
    } yield {
      (".header-title *" #> ?("result-header") &
        ".footer-title *" #> ?("result-footer") &
        ".account-list *" #> fs.getEligibleAccountChoice.toList.foldLeft(NodeSeq.Empty)((acc, mshp) => {
          acc ++ (Templates(List("ajax-templates-hidden", "passwordResult")).map(t => {
            (".account-id *" #> mshp.external_id &
              ".account-scheme *" #> mshp.scheme &
              ".account-status *" #> mshp.status &
              ".account-result *" #> (factProvider.getAccount(mshp.external_id) match {
                case Right(accountDefinition) => Text(?("account-reset"))
                case Left(e)                  => Text(?("account-not-reset"))
              })).apply(t)
          })).openOr(NodeSeq.Empty)
        })).apply(template)
    }).openOr(NodeSeq.Empty)
  }

  protected def provideError(errorMessage: String): NodeSeq = {
    currentStage(Some(Summary))
    Templates(List("ajax-templates-hidden", "provideError")).map(t => {
      (".error-text *" #> Text(errorMessage) &
        startOver(".btn-restart [onclick]", "/")).apply(t)
    }).openOr(NodeSeq.Empty)
  }

  protected def generateCurrentPageNodeSeq: NodeSeq = {
    val node = currentFactSet.is match {
      case None => askForMembershipNumber
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
        provideError(?("call-cic"))
      }
      case Some(factSet) if !factSet.canComplete => {
        trace("%s does not have enough facts avialable to complete the process.".format(serviceNumber.is.getOrElse("")))
        provideError(?("call-cic"))
      }
      case Some(factSet) if !factSet.getHasChosen => {
        trace("%s has only one verification method, skipping the associated choice screen.".format(serviceNumber.is.getOrElse("")))
        askForVerificationMethod(factSet)
      }
      case Some(factSet) if factSet.isComplete && !factSet.getHasChosenEligibleAccount &&
        factSet.getEligibleMemberships.size > 1 => {

        trace("%s has more than one eligible account for registration / reset.".format(serviceNumber.is.getOrElse("")))
        askForAccounts
      }
      case Some(factSet) if factSet.isComplete && !factSet.getHasChosenEligibleAccount &&
        factSet.getEligibleMemberships.size == 1 => {

        trace("%s has only one eligible account for registration / reset. Skipping choice.".format(serviceNumber.is.getOrElse("")))
        factSet.setEligibleAccountChoice(factSet.getEligibleMemberships)
        pushUserAction()
        askForPassword
      }
      case Some(factSet) if factSet.isComplete &&
        factSet.getHasChosenEligibleAccount && (SessionState.userPassword.is.getOrElse("") != "") => {

        trace("%s has had eligible accounts registered / reset.".format(serviceNumber.is.getOrElse("")))
        providePasswords
      }
      case Some(factSet) if factSet.isComplete && factSet.getHasChosenEligibleAccount => {

        trace("%s is being prompted to setup a password.".format(serviceNumber.is.getOrElse("")))
        askForPassword
      }
      case Some(factSet) => {

        trace("%s is being challenged to prove their identity.".format(serviceNumber.is.getOrElse("")))
        askForProofOfIdentity(factSet)
      }
    }
    (".btn-get-started-text *" #> ?("btn-get-started-text") &
      ".btn-reset-text *" #> ?("btn-reset-text") &
      ".btn-restart-text *" #> ?("btn-restart-text") &
      ".btn-next-text *" #> ?("btn-next-text") &
      startOver(".btn-restart [onclick]", "/") &
      startOver()).apply(node)
  } ++ Script(setCurrentStage) ++ Script(focusFirstInputField)
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
