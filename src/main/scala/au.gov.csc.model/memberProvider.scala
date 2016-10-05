package au.gov.csc.model

import java.util.Date

import scala.xml.{ NodeSeq, Text }

trait MemberProvider {

  def getMember(memberNumber: String): Either[Exception, Member]
}

class MockMemberProvider extends MemberProvider {

  val MemberNotFoundException = new Exception("member not found")
  val memberFacts: Map[String, Member] = Map()

  override def getMember(memberNumber: String): Either[Exception, Member] = memberFacts.get(memberNumber) match {
    case Some(m) => Right(m)
    case None    => Left(MemberNotFoundException)
  }
}
