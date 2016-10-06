package au.gov.csc.model.fact

import au.gov.csc.model.member._
import au.gov.csc.model.account._

trait FactProvider {

  val EligibleMembershipsNotFoundException = new Exception("No eligible memberships found")

  def getFacts(memberNumber: String): Either[Exception, Member]
  def getAccount(memberNumber: String): Either[Exception, AccountDefinition]
}
