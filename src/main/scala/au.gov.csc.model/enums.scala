package au.gov.csc.model

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
