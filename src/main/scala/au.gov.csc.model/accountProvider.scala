package au.gov.csc.snippet

import java.util.Date

import scala.xml.{ NodeSeq, Text }

case class AccountDefinition(
  memberNumber: String,
  password: String,
  scheme: String
)

trait AccountProvider {

  def getAccount(memberNumber: String): Either[Exception, AccountDefinition]
}

class MockAccountProvider extends AccountProvider {

  val AccountNotFoundException = new Exception("account not found")
  val accounts = Map(
    "77929555" -> AccountDefinition("77929555", "testPassword", "testScheme")
  )

  override def getAccount(memberNumber: String): Either[Exception, AccountDefinition] =
    accounts.get(memberNumber) match {
      case Some(a) => Right(a)
      case None    => Left(AccountNotFoundException)
    }
}