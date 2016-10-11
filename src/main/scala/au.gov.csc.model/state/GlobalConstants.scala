package au.gov.csc.model.state

import au.gov.csc.model.fact._
import au.gov.csc.model.scheme._

class GlobalConstants {
  var minimumCorrectTwoFactorAnswers = 2
  var minimumCorrectNonTwoFactorAnswers = 3
  var questionsPerPage = 2
  var navigationTopic: String = "serverSync"
  var tokenTopic: String = "serverSync"
  var accessAttemptTopic: String = "serverSync"
  var navigationSubscription: String = "serverSync"
  var tokenSubscription: String = "serverSync"
  var accessAttemptSubscription: String = "serverSync"
}