package au.gov.csc.model.fact

import net.liftweb.util.Helpers._
import au.gov.csc.model._
import au.gov.csc.model.question._
import au.gov.csc.model.member._
import au.gov.csc.model.state._

trait FactSet {
  val factSetId: String = nextFuncName
  def getHasChosen: Boolean
  def getChoices: Seq[WorkflowTypeChoice.Value]
  def setChoice(choice: WorkflowTypeChoice.Value)
  def getNextQuestions: Option[QuestionSet]
  def answerQuestions(answers: Seq[QuestionAnswer])
  def isComplete: Boolean
  def canComplete: Boolean
  def getCurrentEmail: String
  def getCurrentMobileNumber: String
  def getRemainingUnansweredQuestionCount: Int
  def getEligibleMemberships: Seq[Membership]
  def setEligibleAccountChoice(mshps: Seq[Membership])
  def getHasChosenEligibleAccount: Boolean
  def getEligibleAccountChoice: Seq[Membership]
}
