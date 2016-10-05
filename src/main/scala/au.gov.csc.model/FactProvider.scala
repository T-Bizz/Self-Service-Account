package au.gov.csc.model

trait FactProvider {

  val EligibleMembershipsNotFoundException = new Exception("No eligible memberships found")

  def getFacts(memberNumber: String): Either[Exception, Member]
  def getAccount(memberNumber: String): Either[Exception, AccountDefinition]
}

class MockFactProvider extends FactProvider {

  val mockMemberProvider = new MockMemberProvider
  val mockAccountProvider = new MockAccountProvider

  def getFacts(memberNumber: String): Either[Exception, Member] =
    mockMemberProvider.getMember(memberNumber)

  def getAccount(memberNumber: String): Either[Exception, AccountDefinition] =
    mockAccountProvider.getAccount(memberNumber)
}

class DataDrivenMockFactProvider(facts: List[Tuple3[String, Member, AccountDefinition]]) extends MockFactProvider {
  override val mockMemberProvider = new MockMemberProvider() {
    override val memberFacts = Map(facts.map(f => (f._1, f._2)): _*)
  }
  override val mockAccountProvider = new MockAccountProvider() {
    override val accounts = Map(facts.map(f => (f._1, f._3)): _*)
  }
}