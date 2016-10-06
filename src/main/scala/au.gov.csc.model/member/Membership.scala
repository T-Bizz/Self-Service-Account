package au.gov.csc.model.member

import java.util.Date

case class Membership(
  external_id: String,
  scheme: String,
  status: String,
  joinDate: Date,
  exitDate: Option[Date]
)