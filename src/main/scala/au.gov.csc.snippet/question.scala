package au.gov.csc.snippet

import net.liftweb.common.Loggable
import net.liftweb.util.Helpers._

object question extends Loggable {
  var (numberOfQuestions: Int, currentQuestion) = (6, 0)

  def render = {
    incrementQuestion
    val questionTitle: String = title(currentQuestion)
    "#question-label *" #> questionTitle
  }

  def incrementQuestion = {
    currentQuestion += 1
    if (currentQuestion > numberOfQuestions) {
      currentQuestion = 1
    }
  }

  def title(questionNumber: Int): String = questionNumber match {
    case 1 => "Question 1"
    case 2 => "Question 2"
    case 3 => "Question 3"
    case 4 => "Question 4"
    case 5 => "Question 5"
    case 6 => "Question 6"
  }

}