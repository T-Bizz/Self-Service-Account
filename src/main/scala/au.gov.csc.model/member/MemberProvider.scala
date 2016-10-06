package au.gov.csc.model.member

trait MemberProvider {

  def getMember(memberNumber: String): Either[Exception, Member]
}