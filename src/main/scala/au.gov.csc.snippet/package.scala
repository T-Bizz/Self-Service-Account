package au.gov.csc

import au.gov.csc.snippet.MockFactProvider
import net.liftweb.http.SessionVar

object SessionState {

  var minimumCorrectAnswers = 3
  var pageSize = 2

  object userState extends SessionVar[Option[String]](None)
  val userProvider = new MockFactProvider()//new MockUserProvider()


  // user state during their workflow
  object currentStep extends SessionVar[Int](0)
  object numberOfSteps extends SessionVar[Int](5)
  object numberOfQuestionsPerPage extends SessionVar[Int](3)
  object routeNumber extends SessionVar[Int](0)
  object skipTwoFactorStep extends SessionVar[Boolean](true)
  object serviceNumber extends SessionVar[String]("")
  object title extends SessionVar[String]("")
}
