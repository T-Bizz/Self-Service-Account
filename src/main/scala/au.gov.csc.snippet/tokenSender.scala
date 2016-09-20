package au.gov.csc.snippet

import net.liftweb.common._

trait TokenSender extends Logger {
  def send(target:Either[EmailAddress,PhoneNumber],token:String):Option[Exception]
}

class MockTokenSender extends TokenSender {
  override def send(target:Either[EmailAddress,PhoneNumber],token:String):Option[Exception] = {
    println("sent token (%s) to %s".format(token,target))
    None
  }
}


class ConcreteTokenSender(emailTokenSender:Option[EmailTokenSender] = None, smsTokenSender:Option[SmsTokenSender] = None) extends TokenSender {
  override def send(target:Either[EmailAddress,PhoneNumber],token:String):Option[Exception] = {
    target match {
      case Left(email) => emailTokenSender.flatMap(_.send(target,token))
      case Right(phone) => emailTokenSender.flatMap(_.send(target,token))
    }
  }
}

class SmsTokenSender extends TokenSender {
  override def send(target:Either[EmailAddress,PhoneNumber],token:String):Option[Exception] = {
    Some(new Exception("sms token sender not yet implemented"))
  }
}
class EmailTokenSender extends TokenSender {
  override def send(target:Either[EmailAddress,PhoneNumber],token:String):Option[Exception] = {
    Some(new Exception("email token sender not yet implemented"))
  }
}
