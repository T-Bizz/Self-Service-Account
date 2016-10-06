package au.gov.csc.model.question

object QuestionSetType extends Enumeration {

  type QuestionSetType = Value

  val TokenEmail, TokenSMS, Hygiene, CurrentMembership, ContactDetails = Value
}
