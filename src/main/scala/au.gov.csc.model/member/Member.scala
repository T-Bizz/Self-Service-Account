package au.gov.csc.model.member

case class Member(
  person: Person,
  memberships: Seq[Membership],
  contactDetails: Seq[ContactDetail]
)