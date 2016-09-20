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
        Some("Mr"),
        Some("87654321")
      ),
      List(
        Membership("75431657",
          "CSS",
          "Preserved",
          new Date(),
          None,
          Some(new Date())),
        Membership("77929551",
          "CSS",
          "Subsumed",
          new Date(),
          Some(new Date()),
          Some(new Date()))
      ),
      List(
        PhoneNumber(
          "mobile",
          "11111111",
          "22222222",
          true,
          new Date(),
          new Date(),
          Some(new Date())
        ),
        EmailAddress(
          "internet",
          "tom@tom.com",
          true,
          new Date(),
          new Date(),
          Some(new Date())),
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
        Some("Mr"),
        Some("87654321")
      ),
      List(
        Membership("77929555",
          "CSS",
          "Preserved",
          new Date(),
          None,
          Some(new Date())),
        Membership("77929551",
          "CSS",
          "Subsumed",
          new Date(),
          Some(new Date()),
          Some(new Date()))
      ),
      List(
        PhoneNumber(
          "mobile",
          "11111111",
          "22222222",
          true,
          new Date(),
          new Date(),
          Some(new Date())
        ),
        EmailAddress(
          "internet",
          "tom@tom.com",
          true,
          new Date(),
          new Date(),
          Some(new Date())),
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
