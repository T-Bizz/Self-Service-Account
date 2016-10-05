package au.gov.csc.model

trait ContactDetail

case class PhoneNumber(
  kind: String,
  countryCode: String,
  areaCode: String,
  phoneNumber: String,
  isValid: Boolean
)
    extends ContactDetail

case class EmailAddress(
  kind: String,
  address: String,
  isValid: Boolean
)
    extends ContactDetail

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