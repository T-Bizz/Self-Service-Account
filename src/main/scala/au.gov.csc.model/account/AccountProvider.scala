package au.gov.csc.model.account

trait AccountProvider {

  def getAccount(memberNumber: String): Either[Exception, AccountDefinition]
}
