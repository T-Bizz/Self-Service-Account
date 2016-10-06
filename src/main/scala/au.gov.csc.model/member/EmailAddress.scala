package au.gov.csc.model.member

case class EmailAddress(
  kind: String,
  address: String,
  isValid: Boolean
)
    extends ContactDetail