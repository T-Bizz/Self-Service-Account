package au.gov.csc.model

import net.liftweb.http.SessionVar

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.common._

object Globals {
  var userProvider: FactProvider = new MockFactProvider()
  var tokenSender: TokenSender = new MockTokenSender()
  var tokenGenerator: TokenGenerator = new NextFuncNameTokenProvider()
}

object SessionState {

  var minimumCorrectAnswers = 3
  var pageSize = 2

  object userState extends SessionVar[Option[String]](None)

  // user state during their workflow
  object currentStep extends SessionVar[Int](0)
  object numberOfSteps extends SessionVar[Int](5)
  object numberOfQuestionsPerPage extends SessionVar[Int](3)
  object routeNumber extends SessionVar[Int](0)
  object skipTwoFactorStep extends SessionVar[Boolean](true)
  //object serviceNumber extends SessionVar[String]("")
  object title extends SessionVar[String]("")

  object Scheme extends RequestVar[Option[Tuple3[String, String, String]]](None)

  object serviceNumber extends SessionVar[Option[String]](None)
  object currentFactSet extends SessionVar[Option[FactSet]](None)
  object currentAccountDetails extends SessionVar[Option[AccountDefinition]](None)
  object currentStage extends SessionVar[Option[StageTypeChoice.Value]](None)
}
