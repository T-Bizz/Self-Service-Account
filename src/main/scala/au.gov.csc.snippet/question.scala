package au.gov.csc.snippet

import net.liftweb.common.Loggable
import net.liftweb.util.Helpers._

object question extends Loggable {
  var (numberOfQuestions: Int, currentQuestion) = (6, 0)

  def render = {
    incrementQuestion
    val questionTitle: String = f"Question #$currentQuestion"
    "#question-label *" #> questionTitle
  }

  def incrementQuestion = {
    currentQuestion += 1
    if (currentQuestion > numberOfQuestions) {
      currentQuestion = 1
    }
  }
}