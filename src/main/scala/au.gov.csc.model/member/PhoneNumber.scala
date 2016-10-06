package au.gov.csc.model.member

case class PhoneNumber(
  kind: String,
  countryCode: String,
  areaCode: String,
  phoneNumber: String,
  isValid: Boolean
)
    extends ContactDetail