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

  def selectCurrentStage: JsCmd = {
    val setActiveStep: JsCmd = currentStage.is match {
      case Some(StageTypeChoice.Verify) => JsCmds.Run("jQuery('#li-step-2').addClass('active')")
      case Some(StageTypeChoice.Result) => JsCmds.Run("jQuery('#li-step-3').addClass('active')")
      case None | Some(_) | Some(StageTypeChoice.Identify) => JsCmds.Run("jQuery('#li-step-1').addClass('active')")
    }

    JsCmds.Run("jQuery('.step-heading').addClass('disabled').removeClass('active')") &
      setActiveStep
  }

  def addValidationMarkup(isTrue: Boolean): JsCmd = {
    if (isTrue) {
      JsCmds.Run("jQuery('#form-group-serviceNumber').removeClass('has-error').addClass('has-success')")
    } else {
      JsCmds.Run("jQuery('#form-group-serviceNumber').removeClass('has-success').addClass('has-error')")
    }
  }

  def askForMemberNumber = Templates(List("ajax-templates-hidden","AskForMemberNumber")).map(t => {
    currentStage(Some(Identify))
    ("#serviceNumber" #> ajaxText(serviceNumber.is.getOrElse(""), s => {
      serviceNumber(Some(s))
      Noop
    }) &
      /*
      "input [onchange]" #> SHtml.onEvent( answer =>
        answer match {
          case _ => addValidationMarkup(Random.nextBoolean())
        }
      ) &
      */
      ".submitButton [onclick]" #> ajaxCall(JsRaw("this"),(_s:String) => {
        serviceNumber.is.map(s => {
          factProvider.getFacts(s) match {
            case Right(member) => {
              currentFactSet(Some(new MemberBackedFactSet(member, SessionState.minimumCorrectAnswers, SessionState.pageSize)))
              generateCurrentPageNodeSeq.map(newPage => SetHtml(contentAreaId, newPage) & selectCurrentStage).getOrElse(Alert("error"))
            }
            case Left(e) => {
              Alert(e.getMessage)
            }
          }
        }).getOrElse(Alert("please provide a member number"))
      })
      ).apply(t)
  })

  def provideAccountDetails = {
    // HOW DO I USE selectCurrentStage here???????????????????????
    currentStage(Some(Result))
    for {
      template <- Templates(List("ajax-templates-hidden","provideAccountNumber"))
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
    }
  }

  def challengeFactSet(factSet:FactSet) = {
    currentStage(Some(Verify))
    factSet.getNextQuestions match {
      case Some(questionSet) => {
        var potentialAnswers:List[Answer] = Nil
        Templates(List("ajax-templates-hidden","QuestionSet")).map(qst => {
          (
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

                questionTemplate.map(qt => {
                  acc ++ ((
                    ".question-title *" #> question.title &
                      ".question-input [onchange]" #> ajaxCall(JsRaw("this.value"), answerQuestionFunc) &
                      ".question-input [placeholder]" #> question.placeHolder &
                      ".question-help-text [data-content]" #> question.helpText
                    ).apply(qt))
                }).openOr(NodeSeq.Empty)
              }) &
              ".submitButton [onclick]" #> ajaxCall(JsRaw("this"),(s:String) => {
                factSet.answerQuestions(potentialAnswers)
                generateCurrentPageNodeSeq.map(newPage => SetHtml(contentAreaId, newPage) & selectCurrentStage).getOrElse(Alert("error"))
              }) &
              startOver
            ).apply(qst)
        })
      }
      case None => {
        Full(Text("We couldn't verify your identity. Contact our Customer Information Center (CIC)."))
      }
    }
  }

  protected def generateCurrentPageNodeSeq:Box[NodeSeq] = {
    currentFactSet.is match {
      case None => askForMemberNumber
      case Some(factSet) if !factSet.canComplete => Full(Text("We can't verify your identity. Contact our Customer Information Center (CIC)."))
      case Some(factSet) if factSet.isComplete => provideAccountDetails
      case Some(factSet) => challengeFactSet(factSet)
    }
  }

  def startOver: CssSel = {
    "#reset [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
        S.session.foreach(s => {
          s.destroySession()
          s.httpSession.foreach(httpsession => {
            httpsession.terminate
          })
        })
        generateCurrentPageNodeSeq.map(newPage => SetHtml(contentAreaId, newPage) & selectCurrentStage).getOrElse(Alert("reset error"))
      })
  }

  def render = {
    // HOW DO I USE selectCurrentStage here???????????????????????
    "#%s *".format(contentAreaId) #> generateCurrentPageNodeSeq
  }
}