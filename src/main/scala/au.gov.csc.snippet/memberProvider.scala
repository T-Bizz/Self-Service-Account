package au.gov.csc.snippet

import java.util.Date

import scala.xml.{NodeSeq,Text}


case class Member(person: Person,
                  memberships: Seq[Membership],
                  contactDetails: Seq[ContactDetail])

case class Person(surname: String,
                  firstName: String,
                  dob: Date,
                  age: Int,
                  fullname: String,
                  title: Option[String],
                  tfn: Option[String])

case class Membership(membershipNumber: String,
                      scheme: String,
                      status: String,
                      joinDate: Date,
                      exitDate: Option[Date],
                      effectDate: Option[Date])
trait ContactDetail

case class PhoneNumber(kind: String,
                       areaCode: String,
                       phoneNumber: String,
                       isValid: Boolean,
                       effectDate: Date,
                       startDate: Date,
                       endDate: Option[Date])
  extends ContactDetail

case class EmailAddress(kind: String,
                        emailAddress: String,
                        isValid: Boolean,
                        effectDate: Date,
                        startDate: Date,
                        endDate: Option[Date])
  extends ContactDetail

case class Address(kind: String,
                   address: String,
                   isValid: Boolean,
                   effectDate: Date,
                   startDate: Date,
                   endDate: Option[Date])
  extends ContactDetail

case class ComplexAddress(kind: String,
                          streetNumber: Int,
                          city: String,
                          state: String,
                          country: String,
                          postCode: String,
                          isValid: Boolean,
                          effectDate: Date,
                          startDate: Date,
                          endDate: Option[Date])
  extends ContactDetail

trait MemberProvider {

  def getMember(memberNumber:String):Either[Exception,Member]
}

class MockMemberProvider extends MemberProvider {

  val MemberNotFoundException = new Exception("member not found")
  val memberFacts = Map(
    "77929555" -> Member(Person("Smith","John",new Date(),21,"John Smith",Some("Mr"),Some("87654321")),Nil,
      List(PhoneNumber("mobile",
        "11111111",
        "22222222",
        true,
        new Date(),
        new Date(),
        Some(new Date())),
        EmailAddress("internet",
          "tom@tom.com",
          true,
          new Date(),
          new Date(),
          Some(new Date()))))
  )

  override def getMember(memberNumber:String):Either[Exception,Member] = memberFacts.get(memberNumber) match {
    case Some(m) => Right(m)
    case None => Left(MemberNotFoundException)
  }
}

