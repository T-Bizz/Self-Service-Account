package au.gov.csc.model

import java.util.Date

import scala.xml.{ NodeSeq, Text }

object WorkflowTypeChoice extends Enumeration {

  type WorkflowTypeChoice = Value

  val QuestionsOnly, SmsAndQuestions, EmailAndQuestions = Value
}

object StageTypeChoice extends Enumeration {

  type StageTypeChoice = Value

  val Identify, Verify, SetPassword, Summary = Value
}

object QuestionSetType extends Enumeration {

  type QuestionSetType = Value

  val TokenEmail, TokenSMS, Hygiene, CurrentMembership, ContactDetails = Value
}
