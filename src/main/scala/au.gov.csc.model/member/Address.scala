package au.gov.csc.model.member

case class Address(
  kind: String,
  line1: String,
  line2: String,
  line3: String,
  city: String,
  state: String,
  country: String,
  postCode: String,
  isValid: Boolean
)
    extends ContactDetail