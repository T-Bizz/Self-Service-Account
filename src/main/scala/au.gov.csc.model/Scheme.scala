package au.gov.csc.model

trait SchemeDefinition {
  val key: String
  val shortCode: String
  val publicWebsite: String
  val logo: String
  val loginScreen: String
}

class Scheme(
  override val key: String,
  override val shortCode: String,
  override val publicWebsite: String,
  override val logo: String,
  override val loginScreen: String
)
    extends SchemeDefinition