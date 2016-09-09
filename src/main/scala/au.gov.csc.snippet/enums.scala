package au.gov.csc.snippet

import java.util.Date

import scala.xml.{NodeSeq,Text}

object WorkflowTypeChoice extends Enumeration {

  type WorkflowTypeChoice = Value

  val QuestionsOnly, SmsAndQuestions, EmailAndQuestions = Value
}

object StageTypeChoice extends Enumeration {

  type StageTypeChoice = Value

  val Identify, Verify, Result = Value
}