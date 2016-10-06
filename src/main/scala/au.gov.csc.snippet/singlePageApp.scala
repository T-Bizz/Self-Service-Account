package au.gov.csc.snippet

import au.gov.csc.model._
import au.gov.csc.model.question._
import au.gov.csc.model.member._
import au.gov.csc.model.fact._
import au.gov.csc.model.state._
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

  protected def setCurrentStage: JsCmd = SessionState.currentStage.is match {
    case Some(StageTypeChoice.Verify)                    => setCurrentStage("2")
    case Some(StageTypeChoice.SetPassword)               => setCurrentStage("3")
    case Some(StageTypeChoice.Summary)                   => setCurrentStage("4")
    case None | Some(_) | Some(StageTypeChoice.Identify) => setCurrentStage("1")
  }

  protected def ?(key: String): String =
    S.?("%s%s".format(key.toLowerCase, SessionState.Scheme.is.map(s => "-%s".format(s.shortCode)).getOrElse("").toLowerCase)) match {
      case out if out == "%s%s".format(key.toLowerCase, SessionState.Scheme.is.map(s => "-%s".format(s.shortCode)).getOrElse("").toLowerCase) => {
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
          trace("Redirecting to (%s) for %s".format(p, SessionState.serviceNumber.is))
          ajaxCall(JsRaw("this"), (_s: String) => {
            RedirectTo(p)
          })
        case None =>
          trace("Redirecting to current page for %s".format(SessionState.serviceNumber.is))
          ajaxCall(JsRaw("this"), (s: String) => {
            SetHtml(contentAreaId, generateCurrentPageNodeSeq)
          })
      }
    }
  }

  protected def initQuestion(template: NodeSeq, title: String, placeholder: String, helpText: String, icon: String, onChange: JsCmd, action: Option[JsCmd] = None, actionTitle: Option[String] = None): NodeSeq = {
    trace("Displaying question (%s) for %s".format(title, SessionState.serviceNumber.is))
    (".form-group [id]" #> nextFuncName &
      ".question-title *" #> title &
      ".question-input [title]" #> title &
      ".question-input [placeholder]" #> placeholder &
      ".question-help-text [data-content]" #> helpText &
      ".question-help-text .sr-only *" #> helpText &
      ".question-icon [class+]" #> icon &
      ".question-input [onchange]" #> onChange &
      ".action-group" #> {
        ".question-ask-action [onclick]" #> action.getOrElse(Noop) &
          ".question-ask-action-label *" #> actionTitle.getOrElse("")
      }).apply(template)
  }

  protected def startOver(
    csssel: String = ".btn-reset [onclick]",
    redirect: String = "/scheme/%s".format(getScheme.map(p => p.shortCode).getOrElse("").toUpperCase)
  ): CssSel = {
    trace("Destroying session at users request for %s".format(SessionState.serviceNumber.is))
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

  protected def obscure(text: String) = "*" * text.length

  protected def obfuscatePhoneNumber(in: String): String = in match {
    case "unknown"         => in
    case i if i.length > 2 => obscure(i.substring(0, i.length - 2)) + i.substring(i.length - 2)
    case i                 => i
  }

  protected def obfuscateEmailAddress(in: String): String = {
    val shortMailbox = "(.{1,2})".r
    val longMailbox = "(.)(.*)(.)".r
    val validDomain = """^([a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*)$""".r
    val validEmail = """^([a-zA-Z0-9.!#$%&’'*+/=?^_`{|}~-]+)@([a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*)$""".r

    if (in.split("@").toList.size >= 2) {
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
    } else {
      "unknown"
    }
  }

  protected def askForMembershipNumber: NodeSeq = Templates(List("ajax-templates-hidden", "askForMembershipNumber")).map(t => {
    trace("Generating page askForMembershipNumber")
    SessionState.currentStage(Some(Identify))
    (".header-title *" #> ?("identify-header") &
      ".sub-header-title *" #> ?("identify-sub-header") &
      ".footer-title *" #> ?("identify-footer") &
      ".btn-submit [onclick]" #> ajaxCall(JsRaw("jQuery('#serviceNumber').val()"), (s: String) => {
        SessionState.serviceNumber(Some(s))
        if (SessionState.currentFactSet.is.isDefined) {
          showModalError(?("error-title-invalid-data"), ?("membership-number-already-provided")) &
            SetHtml(contentAreaId, generateCurrentPageNodeSeq)
        } else {
          val mn: MembershipNumber = new MshpNumber(s)
          SessionState.serviceNumber.is.map(s => {
            new MshpNumber(SessionState.serviceNumber.is.getOrElse("")).isValid match {
              case true => factProvider.getFacts(s) match {
                case Right(member) => {
                  try {
                    SessionState.currentFactSet(Some(new MemberBackedFactSet(member)))
                  } catch {
                    case t: Throwable =>
                      error("exception: %s\r\n%s".format(t.getMessage, t.getStackTrace))
                  }
                  pushUserAction()
                  SetHtml(contentAreaId, generateCurrentPageNodeSeq)
                }
                case Left(e) => {
                  showModalError(?("error-title"), ?(e.getMessage)) &
                    addValidationMarkup("#form-group-serviceNumber", mn.isValid, mn.validate.headOption.getOrElse(""), "Membership Number ")
                }
              }
              case false => showModalError(?("error-title-invalid-data"), ?("invalid-nembership-number-provided")) &
                addValidationMarkup("#form-group-serviceNumber", mn.isValid, mn.validate.headOption.getOrElse(""), "Membership Number ")
            }
          }).getOrElse(showModalError(?("error-title-missing-data"), ?("no-membership-number-provided")))
        }
      })).apply(t)
  }).openOr(NodeSeq.Empty)

  protected def askForVerificationMethod(factSet: FactSet): NodeSeq = {
    trace("Generating page askForVerificationMethod for %s".format(SessionState.serviceNumber.is))
    SessionState.currentStage(Some(Verify))
    (for {
      template <- Templates(List("ajax-templates-hidden", "askForVerificationMethod"))
      memberNumber <- SessionState.serviceNumber.is
    } yield {
      var currentChoice: Option[WorkflowTypeChoice.Value] = None
        def initButton(template: NodeSeq, option: WorkflowTypeChoice.Value, value: Option[String]): NodeSeq = {
          factSet.getChoices.contains(option) match {
            case true => (".btn-value *" #> value.getOrElse("") &
              ".list-group-item [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
                currentChoice = Some(option)
                Noop
              })).apply(template)
            case false => NodeSeq.Empty
          }
        }
      (".header-title *" #> ?("verification-method-choice-header") &
        ".sub-header-title *" #> ?("verification-method-choice-sub-header") &
        ".footer-title *" #> ?("verification-method-choice-footer") &
        "#btn-phone" #> ((n: NodeSeq) => {
          initButton(n, WorkflowTypeChoice.SmsAndQuestions, Some(obfuscatePhoneNumber(factSet.getCurrentMobileNumber)))
        }) &
        "#btn-email" #> ((n: NodeSeq) => {
          initButton(n, WorkflowTypeChoice.EmailAndQuestions, Some(obfuscateEmailAddress(factSet.getCurrentEmail)))
        }) &
        "#btn-other" #> ((n: NodeSeq) => {
          initButton(n, WorkflowTypeChoice.QuestionsOnly, None)
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
    trace("Generating page askForProofOfIdentity for %s".format(SessionState.serviceNumber.is))
    SessionState.currentStage(Some(Verify))
    factSet.getNextQuestions match {
      case Some(questionSet) => {
        var potentialAnswers: List[QuestionAnswer] = Nil
        Templates(List("ajax-templates-hidden", "askForProofOfIdentity")).map(qst => {
          (".question-set-header *" #> ?("question-set-heading") &
            ".question-set-sub-header *" #> ?("question-set-sub-heading") &
            ".question-set-footer *" #> questionSet.footer &
            ".question-set-title *" #> questionSet.title &
            ".questions *" #> questionSet.questions.toList.foldLeft(NodeSeq.Empty)((acc, question) => {
              potentialAnswers = QuestionAnswer("", question) :: potentialAnswers

              val answerQuestionFunc = (answerString: String) => {
                potentialAnswers = potentialAnswers.filterNot(pa => {
                  pa.question == question
                })
                potentialAnswers = QuestionAnswer(answerString, question) :: potentialAnswers
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
                initQuestion(
                  qt,
                  question.title.toString,
                  question.placeHolder.toString,
                  question.helpText.toString,
                  question.icon.toString,
                  ajaxCall(JsRaw("this.value"), answerQuestionFunc),
                  Some(ajaxCall(JsRaw("this.value"), askQuestionFunc)),
                  question match {
                    case t: TokenQuestion if t.target.isLeft => Some(?("resend-token-to-email"))
                    case t: TokenQuestion                    => Some(?("resend-token-to-mobile"))
                    case _                                   => None
                  }
                )
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
    trace("Generating page askForAccounts for %s".format(SessionState.serviceNumber.is))
    SessionState.currentStage(Some(SetPassword))
    (for {
      template <- Templates(List("ajax-templates-hidden", "askForAccounts"))
      fs <- SessionState.currentFactSet.is
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
    trace("Generating page askForPassword for %s".format(SessionState.serviceNumber.is))
    SessionState.currentStage(Some(SetPassword))
    (for {
      template <- Templates(List("ajax-templates-hidden", "askForPassword"))
      fs <- SessionState.currentFactSet.is
    } yield {
      var password: String = ""
      var score: String = ""
      var passwordConfirmation: String = ""
      (".header-title *" #> ?("ask-for-password-header") &
        ".sub-header-title *" #> ?("ask-for-password-sub-header") &
        ".footer-title *" #> ?("ask-for-password-footer") &
        ".questions *" #> (
          (Templates(List("ajax-templates-hidden", "questionPassword")).map(t => {
            initQuestion(
              t,
              ?("password-question"),
              ?("password-placeholder"),
              ?("password-help-text"),
              ?("password-icon"),
              ajaxCall(JsRaw("zxcvbn($('.question-input:first').val(), user_inputs=[]).score + ':' + this.value"), (s: String) => {
                password = s.split(':').toList.drop(1).mkString
                score = s.split(':').toList.head
                Noop
              })
            )
          })).openOr(NodeSeq.Empty) ++
          (Templates(List("ajax-templates-hidden", "questionPassword")).map(t => {
            initQuestion(
              t,
              ?("password-confirmation-question"),
              ?("password-confirmation-placeholder"),
              ?("password-confirmation-help-text"),
              ?("password-confirmation-icon"),
              ajaxCall(JsRaw("this.value"), (s: String) => {
                passwordConfirmation = s
                Noop
              })
            )
          })).openOr(NodeSeq.Empty)
        ) &
          ".btn-submit [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
            // if password is of required complexity
            if (List("2", "3", "4").contains(score)) {
              if (password == passwordConfirmation) {
                SessionState.userPassword(Some(password))
                pushUserAction()
                SetHtml(contentAreaId, generateCurrentPageNodeSeq)
              } else {
                showModalError(?("error-title-invalid-data"), ?("passwords-do-not-match"))
              }
            } else {
              showModalError(?("error-title-invalid-data"), ?("password-easily-guessable"))
            }
          })).apply(template)
    }).openOr(NodeSeq.Empty)
  }

  protected def providePasswords: NodeSeq = {
    trace("Generating page providePasswords for %s".format(SessionState.serviceNumber.is))
    SessionState.currentStage(Some(Summary))
    (for {
      template <- Templates(List("ajax-templates-hidden", "providePasswords"))
      memberNumber <- SessionState.serviceNumber.is
      fs <- SessionState.currentFactSet.is
      pw <- SessionState.userPassword.is
    } yield {
      (".header-title *" #> ?("result-header") &
        ".sub-header-title *" #> ?("result-sub-header") &
        ".footer-title *" #> ?("result-footer") &
        ".account-list *" #> fs.getEligibleAccountChoice.toList.foldLeft(NodeSeq.Empty)((acc, mshp) => {
          acc ++ (Templates(List("ajax-templates-hidden", "passwordResult")).map(t => {
            (".list-group-item [href]" #> SessionState.Scheme.is.map(s => s.loginScreen).getOrElse("") &
              ".account-id *" #> mshp.external_id &
              ".account-scheme *" #> (mshp.scheme match {
                case "PENSION" => "%s %s".format(?("login-to"), ?("pso-login"))
                case s         => "%s %s %s".format(?("login-to"), ?(s.toLowerCase), ?("mso-login"))
              }) &
              ".account-result *" #> (factProvider.getAccount(mshp.external_id) match {
                case Right(accountDefinition) => Text(?("account-reset"))
                case Left(e)                  => Text(?("account-not-reset"))
              })).apply(t)
          })).openOr(NodeSeq.Empty)
        })).apply(template)
    }).openOr(NodeSeq.Empty)
  }

  protected def provideError(errorMessage: String): NodeSeq = {
    trace("Generating page provideError for %s".format(SessionState.serviceNumber.is))
    SessionState.currentStage(Some(Summary))
    Templates(List("ajax-templates-hidden", "provideError")).map(t => {
      (".header-title *" #> Text(?("error-title-verification-issue")) &
        ".error-text *" #> Text(errorMessage) &
        startOver(".btn-restart [onclick]", "/")).apply(t)
    }).openOr(NodeSeq.Empty)
  }

  protected def generateCurrentPageNodeSeq: NodeSeq = {
    val node = SessionState.currentFactSet.is match {
      case None => askForMembershipNumber
      case Some(factSet) if !factSet.getHasChosen && factSet.getChoices.size == 1 => {
        trace("%s has only one verification method, skipping the associated choice screen.".format(SessionState.serviceNumber.is.getOrElse("")))
        factSet.getChoices.map(choice => {
          factSet.setChoice(choice)
        })
        pushUserAction()
        generateCurrentPageNodeSeq
      }
      case Some(factSet) if !factSet.getHasChosen && factSet.getChoices.size == 0 => {
        warn("%s has no verification methods available.".format(SessionState.serviceNumber.is.getOrElse("")))
        provideError(?("call-cic"))
      }
      case Some(factSet) if !factSet.canComplete => {
        trace("%s does not have enough facts avialable to complete the process.".format(SessionState.serviceNumber.is.getOrElse("")))
        provideError(?("call-cic"))
      }
      case Some(factSet) if !factSet.getHasChosen => {
        trace("%s has only one verification method, skipping the associated choice screen.".format(SessionState.serviceNumber.is.getOrElse("")))
        askForVerificationMethod(factSet)
      }
      case Some(factSet) if factSet.isComplete && !factSet.getHasChosenEligibleAccount &&
        factSet.getEligibleMemberships.size > 1 => {

        trace("%s has more than one eligible account for registration / reset.".format(SessionState.serviceNumber.is.getOrElse("")))
        askForAccounts
      }
      case Some(factSet) if factSet.isComplete && !factSet.getHasChosenEligibleAccount &&
        factSet.getEligibleMemberships.size == 1 => {

        trace("%s has only one eligible account for registration / reset. Skipping choice.".format(SessionState.serviceNumber.is.getOrElse("")))
        factSet.setEligibleAccountChoice(factSet.getEligibleMemberships)
        pushUserAction()
        askForPassword
      }
      case Some(factSet) if factSet.isComplete &&
        factSet.getHasChosenEligibleAccount && (SessionState.userPassword.is.getOrElse("") != "") => {

        trace("%s has had eligible accounts registered / reset.".format(SessionState.serviceNumber.is.getOrElse("")))
        providePasswords
      }
      case Some(factSet) if factSet.isComplete && factSet.getHasChosenEligibleAccount => {

        trace("%s is being prompted to setup a password.".format(SessionState.serviceNumber.is.getOrElse("")))
        askForPassword
      }
      case Some(factSet) => {

        trace("%s is being challenged to prove their identity.".format(SessionState.serviceNumber.is.getOrElse("")))
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

class SinglePageApp extends Logger with SinglePageAppView {
  def comet = {
    val detectedScheme = detectScheme
    val oldScheme = getScheme
    detectedScheme.filterNot(s => oldScheme.exists(_ == s)).foreach(s => {
      trace("detected scheme change: %s => %s".format(oldScheme, s))
      SessionState.Scheme(detectedScheme)
      pushUserAction(Some("/scheme/%s".format(s.shortCode.toUpperCase)))
    })

    val cometActorName = "lift:comet?type=PushActor&name=%s".format(nextFuncName)
    ".app-root-elem [data-lift]" #> cometActorName
  }

  def render = {
    "#%s [data-id]".format(contentAreaId) #> pageId &
      "#%s *".format(contentAreaId) #> { generateCurrentPageNodeSeq }
  }
}
