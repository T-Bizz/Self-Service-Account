package au.gov.csc.model

case class Member(
  person: Person,
  memberships: Seq[Membership],
  contactDetails: Seq[ContactDetail]
)