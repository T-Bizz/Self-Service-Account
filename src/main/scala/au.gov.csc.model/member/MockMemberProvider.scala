package au.gov.csc.model.member

class MockMemberProvider extends MemberProvider {

  val MemberNotFoundException = new Exception("member not found")
  val memberFacts: Map[String, Member] = Map()

  override def getMember(memberNumber: String): Either[Exception, Member] = memberFacts.get(memberNumber) match {
    case Some(m) => Right(m)
    case None    => Left(MemberNotFoundException)
  }
}
