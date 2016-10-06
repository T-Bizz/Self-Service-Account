package au.gov.csc.model.fact

import au.gov.csc.model.member._
import au.gov.csc.model.account._

class MockFactProvider extends FactProvider {

  val mockMemberProvider = new MockMemberProvider
  val mockAccountProvider = new MockAccountProvider

  def getFacts(memberNumber: String): Either[Exception, Member] =
    mockMemberProvider.getMember(memberNumber)

  def getAccount(memberNumber: String): Either[Exception, AccountDefinition] =
    mockAccountProvider.getAccount(memberNumber)
}