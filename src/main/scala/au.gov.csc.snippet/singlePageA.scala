package au.gov.csc.snippet

import net.liftweb.http.{SessionVar, Templates}
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds._
import au.gov.csc._
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.json.JsonAST.JNull

import scala.util.Random
import scala.xml._
/**
  * Created by Tom and Sarah on 6/09/2016.
  */
object serviceNumber extends SessionVar[Option[String]](None)
object currentFactSet extends SessionVar[Option[FactSet]](None)
object currentAccountDetails extends SessionVar[Option[AccountDefinition]](None)
class singlePageApp extends Logger {
  val contentAreaId = "step-form"
  protected var factProvider = SessionState.userProvider
  def addValidationMarkup(isTrue: Boolean): JsCmd = {
    if (isTrue) {
      JsCmds.Run("jQuery('#form-group-serviceNumber').removeClass('has-error').addClass('has-success')")
    } else {
      JsCmds.Run("jQuery('#form-group-serviceNumber').removeClass('has-success').addClass('has-error')")
    }
  }
  def askForMemberNumber = Templates(List("ajax-templates-hidden","AskForMemberNumber")).map(t => {
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
              generateCurrentPageNodeSeq.map(newPage => SetHtml(contentAreaId, newPage)).getOrElse(Alert("error"))
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
    for {
      template <- Templates(List("ajax-templates-hidden","provideAccountNumber"))
      memberNumber <- serviceNumber.is
    } yield {
      factProvider.getAccount(memberNumber) match {
        case Right(accountDefinition) => {
          (
            ".accessNumberValue [value]" #> accountDefinition.password &
              ".serviceNumberValue [value]" #> accountDefinition.memberNumber &
              ".schemeValue [value]" #> accountDefinition.scheme
            ).apply(template)
        }
        case Left(e) => {
          Text(e.getMessage)
        }
      }
    }
  }
  def challengeFactSet(factSet:FactSet) = {
    factSet.getNextQuestions match {
      case Some(questionSet) => {
        var potentialAnswers:List[Answer] = Nil
        Templates(List("ajax-text-snippets-hidden","QuestionSet")).map(qst => {
          (
            ".questionSetTitle" #> questionSet.title &
              ".questionSetFooter" #> questionSet.footer &
              ".questions" #> questionSet.questions.toList.foldLeft(NodeSeq.Empty)((acc,question) => {
                val questionTemplate = question match {
                  case d:DateQuestion => Templates(List("ajax-text-snippets-hidden","DateQuestion"))
                  case s:StringQuestion => Templates(List("ajax-text-snippets-hidden","StringQuestion"))
                }
                questionTemplate.map(qt => {
                  questionSet.questions.toList.foldLeft(NodeSeq.Empty)((acc,question) => {
                    val answerQuestionFunc = (answerString:String) => {
                      question.getValidationErrors(answerString) match {
                        case Nil => {
                          potentialAnswers = Answer(answerString,question) :: potentialAnswers
                          Noop
                        }
                        case other => Alert(other.mkString)
                      }
                    }
                    acc ++ ((
                      ".title" #> question.title &
                        ".questionInput [onchange]" #> ajaxCall(JsRaw("this.value"),answerQuestionFunc) &
                        ".questionInput [placeholder]" #> question.placeHolder &
                        ".helpText *" #> question.helpText
                      ).apply(qt))

                  })
                }).openOr(NodeSeq.Empty)

              }) &
              ".submitButton [onclick]" #> ajaxCall(JsRaw("this"),(s:String) => {
                factSet.answerQuestions(potentialAnswers)
                generateCurrentPageNodeSeq.map(newPage => SetHtml(contentAreaId,newPage)).getOrElse(Alert("error"))
              })
            ).apply(qst)
        })
      }
      case None => {
        Full(Text("we couldn't verify you.  Call CSC CIC"))
      }
    }
  }
  protected def generateCurrentPageNodeSeq:Box[NodeSeq] = {
    currentFactSet.is match {
      case None => askForMemberNumber
      case Some(factSet) if !factSet.canComplete => Full(Text("we can't verify you online.  Please contact CSC CIC"))
      case Some(factSet) if factSet.isComplete => provideAccountDetails
      case Some(factSet) => challengeFactSet(factSet)
    }

  }
  def render = {
    "#%s *".format(contentAreaId) #> generateCurrentPageNodeSeq &
    "#reset [onclick]" #> ajaxCall(JsRaw("this"), (s: String) => {
      S.session.foreach(s => {
        s.destroySession()
        s.httpSession.foreach(httpsession => {
          httpsession.terminate
        })
      })
      RedirectTo("/singlePageApp")
    })
  }
}
