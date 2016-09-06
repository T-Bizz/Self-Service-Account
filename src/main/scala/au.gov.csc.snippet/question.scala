package au.gov.csc.snippet

import net.liftweb.common.Loggable
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object question extends Loggable {
  var (numberOfQuestions: Int, currentQuestion) = (6, 0)

  def render = {
    incrementQuestion
    val questionTitle: NodeSeq = title(currentQuestion)
    val questionPlaceholder: NodeSeq = placeholder(currentQuestion)

    "#question-label *" #> questionTitle &
      "#question-placeholder" #> questionPlaceholder
  }

  def incrementQuestion = {
    currentQuestion += 1
    if (currentQuestion > numberOfQuestions) {
      currentQuestion = 1
    }

    step.routeNumber match {
      case 0 => step.step match {
        case 2 => if (currentQuestion > 3) currentQuestion = 1
        case _ => if (currentQuestion < 4) currentQuestion = 4
      }
      case 1 => step.step match {
        case _ => if (currentQuestion > 3) currentQuestion = 1
      }
    }
  }

  def title(questionNumber: Int): NodeSeq = questionNumber match {
    case 1 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-0"></div>
    case 2 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-1"></div>
    case 3 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-2"></div>
    case 4 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-3"></div>
    case 5 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-4"></div>
    case 6 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-5"></div>
  }

  def placeholder(questionNumber: Int): NodeSeq = questionNumber match {
    case 1 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-0-placeholder"></div>
    case 2 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-1-placeholder"></div>
    case 3 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-2-placeholder"></div>
    case 4 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-3-placeholder"></div>
    case 5 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-4-placeholder"></div>
    case 6 => <div data-lift="embed?what=/ajax-text-snippets-hidden/question-5-placeholder"></div>
  }

}