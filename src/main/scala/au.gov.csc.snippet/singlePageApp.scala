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

import scala.util.Random
import scala.xml._
/**
  * Created by Tom and Sarah on 6/09/2016.
  */
object serviceNumber extends SessionVar[Option[String]](None)
object currentFactSet extends SessionVar[Option[FactSet]](None)
class singlePageApp {
  protected var factProvider = SessionState.userProvider
  def addValidationMarkup(isTrue: Boolean): JsCmd = {
    if (isTrue) {
      JsCmds.Run("jQuery('#form-group-serviceNumber').removeClass('has-error').addClass('has-success')")
    } else {
      JsCmds.Run("jQuery('#form-group-serviceNumber').removeClass('has-success').addClass('has-error')")
    }
  }
  def askForMemberNumber = Templates(List("ajax-templates-hidden","route-0-step-0")).map(t => {
    ("#serviceNumber" #> text(serviceNumber.is.getOrElse(""), s => {
      serviceNumber(Some(s))
      factProvider.getFacts(s) match {
        case Right(member) => currentFactSet(Some(new MemberBackedFactSet(member,SessionState.minimumCorrectAnswers,SessionState.pageSize)))
        case Left(e) => {
          Alert(e.getMessage)
        }
      }
    }) &
      "input [onchange]" #> SHtml.onEvent( answer =>
        answer match {
          case _ => addValidationMarkup(Random.nextBoolean())
        }
      )).apply(t)
  })
  def provideAccountDetails = Full(<div data-lift="embed?what=/ajax-templates-hidden/route-0-step-5"></div>)
  def challengeFactSet(factSet:FactSet) = {
    factSet.getNextQuestions match {
      case Some(questionSet) => {
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
                    val answerQuestionFunc = (answerString:String) => {Noop}
                    acc ++ ((
                      ".title" #> question.title &
                        ".questionInput [onchange]" #> ajaxCall(JsRaw("this.value"),answerQuestionFunc) &
                        ".questionInput [placeholder]" #> question.placeHolder &
                        ".helpText *" #> question.helpText
                      ).apply(qt))

                  })
                }).openOr(NodeSeq.Empty)

              })
            ).apply(qst)
        })
      }
      case None => {
        Full(Text("we couldn't verify you.  Call CSC CIC"))
      }
    }
  }
  def render = {
    "#step-form *" #> {
      currentFactSet.is match {
        case None => askForMemberNumber
        case Some(factSet) if factSet.isComplete => provideAccountDetails
        case Some(factSet) => challengeFactSet(factSet)
      }
    }
  }
}