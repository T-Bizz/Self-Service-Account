package au.gov.csc.model.scheme

class Scheme(
  override val key: String,
  override val shortCode: String,
  override val publicWebsite: String,
  override val logo: String,
  override val loginScreen: String
)
    extends SchemeDefinition