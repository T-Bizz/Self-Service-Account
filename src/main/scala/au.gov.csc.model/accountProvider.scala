package au.gov.csc.model

trait AccountProvider {

  def getAccount(memberNumber: String): Either[Exception, AccountDefinition]
}

class MockAccountProvider extends AccountProvider {

  val AccountNotFoundException = new Exception("account not found")
  val accounts: Map[String, AccountDefinition] = Map()

  override def getAccount(memberNumber: String): Either[Exception, AccountDefinition] =
    accounts.get(memberNumber) match {
      case Some(a) => Right(a)
      case None    => Left(AccountNotFoundException)
    }
}
