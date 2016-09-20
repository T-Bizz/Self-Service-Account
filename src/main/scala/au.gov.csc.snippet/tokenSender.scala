package au.gov.csc.snippet

import net.liftweb.common._

trait TokenSender extends Logger {
  def send(target:Either[EmailAddress,PhoneNumber],token:String,factSet:FactSet):Option[Exception]
}

class MockTokenSender extends TokenSender {
  override def send(target:Either[EmailAddress,PhoneNumber],token:String,factSet:FactSet):Option[Exception] = {
    println("sent token (t: %s sid: %s) to %s".format(token,factSet.factSetId,target))
    None
  }
}


class ConcreteTokenSender(emailTokenSender:Option[EmailTokenSender] = None, smsTokenSender:Option[SmsTokenSender] = None) extends TokenSender {
  override def send(target:Either[EmailAddress,PhoneNumber],token:String,factSet:FactSet):Option[Exception] = {
    target match {
      case Left(email) => emailTokenSender.flatMap(_.send(target,token,factSet))
      case Right(phone) => emailTokenSender.flatMap(_.send(target,token,factSet))
    }
  }
}

class SmsTokenSender extends TokenSender {
  override def send(target:Either[EmailAddress,PhoneNumber],token:String,factSet:FactSet):Option[Exception] = {
    Some(new Exception("sms token sender not yet implemented"))
  }
}
class EmailTokenSender extends TokenSender {
  override def send(target:Either[EmailAddress,PhoneNumber],token:String,factSet:FactSet):Option[Exception] = {
    Some(new Exception("email token sender not yet implemented"))
  }
}
