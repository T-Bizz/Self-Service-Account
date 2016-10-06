package au.gov.csc.model.fact

import au.gov.csc.model.member._
import au.gov.csc.model.account._

class DataDrivenMockFactProvider(facts: List[Tuple3[String, Member, AccountDefinition]]) extends MockFactProvider {
  override val mockMemberProvider = new MockMemberProvider() {
    override val memberFacts = Map(facts.map(f => (f._1, f._2)): _*)
  }
  override val mockAccountProvider = new MockAccountProvider() {
    override val accounts = Map(facts.map(f => (f._1, f._3)): _*)
  }
}