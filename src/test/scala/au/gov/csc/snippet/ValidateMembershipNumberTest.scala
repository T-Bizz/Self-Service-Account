package au.gov.csc.model

import org.specs2._

class ValidateMembershipNumberTest extends org.specs2.mutable.Specification {

  "A membership number" should {
    "must be invalid for standard service number" in {
      val mshp: MembershipNumber = new MshpNumber("A12345")
      mshp.isValid must beTrue
    }

    "must be invalid for standard service number in lower case" in {
      val mshp: MembershipNumber = new MshpNumber("a12345")
      mshp.isValid must beFalse
    }

    "must be invalid for a service number of insuficient length" in {
      val mshp: MembershipNumber = new MshpNumber("A12")
      mshp.isValid must beFalse
    }

    "must be invalid for a service number of to great a length" in {
      val mshp: MembershipNumber = new MshpNumber("A123456789101112v")
      mshp.isValid must beFalse
    }

    "must be invaid for a service number with a CSS pension suffix" in {
      val mshp: MembershipNumber = new MshpNumber("A12345CS")
      mshp.isValid must beFalse
    }

    "must be invalid for a service number with an invalid pensionn suffix" in {
      val mshp: MembershipNumber = new MshpNumber("A12345LA")
      mshp.isValid must beFalse
    }

    "must be invalid for a service number without a pension suffix and with a child / spouse suffix" in {
      val mshp: MembershipNumber = new MshpNumber("A12345A")
      mshp.isValid must beFalse
    }

    "must be valid for a service number with a ADFC suffix and child / spouse suffix" in {
      val mshp: MembershipNumber = new MshpNumber("A12345ADA")
      mshp.isValid must beTrue
    }

    "must be valid for a ags number" in {
      val mshp: MembershipNumber = new MshpNumber("50143894")
      mshp.isValid must beTrue
    }

    "must be invalid for a ags number in the wrong format" in {
      val mshp: MembershipNumber = new MshpNumber("50143895")
      mshp.isValid must beFalse
    }
  }
}