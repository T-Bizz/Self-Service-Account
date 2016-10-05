package au.gov.csc.model

import net.liftweb.http.SessionVar

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.common._

class GlobalConstants {
  var minimumCorrectTwoFactorAnswers = 2
  var minimumCorrectNonTwoFactorAnswers = 3
  var questionsPerPage = 2
}

object Globals {
  var userProvider: FactProvider = new MockFactProvider()
  var tokenSender: TokenSender = new MockTokenSender()
  var tokenGenerator: TokenGenerator = new NextFuncNameTokenProvider()
  var constants: GlobalConstants = new GlobalConstants()
  var schemeList: Map[String, SchemeDefinition] = Map()

  def init(in: Tuple5[FactProvider, TokenSender, TokenGenerator, GlobalConstants, Map[String, SchemeDefinition]]) = {
    userProvider = in._1
    tokenSender = in._2
    tokenGenerator = in._3
    constants = in._4
    schemeList = in._5
  }
}

object SessionState {

  var minimumCorrectAnswers = Globals.constants.minimumCorrectNonTwoFactorAnswers
  var questionsPerPage = Globals.constants.questionsPerPage

  object sessionId extends SessionVar[String](nextFuncName)
  object userState extends SessionVar[Option[String]](None)
  object userPassword extends SessionVar[Option[String]](None)

  // user state during their workflow
  object currentStep extends SessionVar[Int](0)
  object numberOfSteps extends SessionVar[Int](5)
  object numberOfQuestionsPerPage extends SessionVar[Int](3)
  object routeNumber extends SessionVar[Int](0)
  object skipTwoFactorStep extends SessionVar[Boolean](true)
  object title extends SessionVar[String]("")

  object Scheme extends SessionVar[Option[SchemeDefinition]](None)

  object serviceNumber extends SessionVar[Option[String]](None)
  object currentFactSet extends SessionVar[Option[FactSet]](None)
  object currentAccountDetails extends SessionVar[Option[AccountDefinition]](None)
  object currentStage extends SessionVar[Option[StageTypeChoice.Value]](None)
}
