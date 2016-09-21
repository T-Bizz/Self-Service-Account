package au.gov.csc.snippet

import java.util.Date

import scala.xml.{NodeSeq,Text}


case class Member(person: Person,
                  memberships: Seq[Membership],
                  contactDetails: Seq[ContactDetail])

case class Person(surname: String,
                  givenNames: String,
                  birthDate: Date,
                  age: Int,
                  fullName: String,
                  gender: String,
                  title: Option[String],
                  taxFileNumber: Option[String])

case class Membership(external_id: String,
                      scheme: String,
                      status: String,
                      joinDate: Date,
                      exitDate: Option[Date])

trait ContactDetail

case class PhoneNumber(kind: String,
                       countryCode: String,
                       areaCode: String,
                       phoneNumber: String,
                       isValid: Boolean)
  extends ContactDetail

case class EmailAddress(kind: String,
                        address: String,
                        isValid: Boolean)
  extends ContactDetail

case class Address(kind: String,
                   line1: String,
                   line2: String,
                   line3: String,
                   city: String,
                   state: String,
                   country: String,
                   postCode: String,
                   isValid: Boolean)
  extends ContactDetail

trait MemberProvider {

  def getMember(memberNumber:String):Either[Exception,Member]
}

class MockMemberProvider extends MemberProvider {

  val MemberNotFoundException = new Exception("member not found")
  val memberFacts = Map(
    "75431657" -> Member(
      Person(
        "Smith",
        "John",
        new Date(),
        21,
        "John Smith",
        "Male",
        Some("Mr"),
        Some("87654321")
      ),
      List(
        Membership("75431657",
          "CSS",
          "Preserved",
          new Date(),
          None),
        Membership("77929551",
          "CSS",
          "Subsumed",
          new Date(),
          Some(new Date()))
      ),
      List(
        PhoneNumber(
          "mobile",
          "00000000",
          "11111111",
          "22222222",
          true),
        EmailAddress(
          "internet",
          "tom@tom.com",
          true),
        Address(
          "Residential",
          "Unit 4, Chandler St Belconnen",
          "",
          "",
          "Canberra",
          "ACT",
          "Australia",
          "2917",
          true)
      )
    ),
    "77929555" -> Member(
      Person(
        "Smith",
        "John",
        new Date(),
        21,
        "John Smith",
        "Male",
        Some("Mr"),
        Some("87654321")
      ),
      List(
        Membership("77929555",
          "CSS",
          "Preserved",
          new Date(),
          None),
        Membership("77929551",
          "CSS",
          "Subsumed",
          new Date(),
          Some(new Date()))
      ),
      List(
        PhoneNumber(
          "mobile",
          "00000000",
          "11111111",
          "22222222",
          true),
        EmailAddress(
          "internet",
          "tom@tom.com",
          true),
        Address(
          "Residential",
          "Unit 4, Chandler St Belconnen",
          "",
          "",
          "Canberra",
          "ACT",
          "Australia",
          "2917",
          true)
      )
    )
  )

  override def getMember(memberNumber:String):Either[Exception,Member] = memberFacts.get(memberNumber) match {
    case Some(m) => Right(m)
    case None => Left(MemberNotFoundException)
  }
}
