package au.gov.csc.model

import java.util.Date

case class Person(
  surname: String,
  givenNames: String,
  birthDate: Date,
  age: Int,
  fullName: String,
  gender: String,
  title: Option[String],
  taxFileNumber: Option[String]
)