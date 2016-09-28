package au.gov.csc.snippet

import org.specs2._

class ObfuscatedPhoneNumberTest extends org.specs2.mutable.Specification with SinglePageAppView {

  "Obfuscating a phone number" should {
    "work for a long number" in {
      obfuscatePhoneNumber("0123456789") must beEqualTo("********89")
    }

    "work for an invalid number" in {
      obfuscatePhoneNumber("+61+2+2456135235") must beEqualTo("**************35")
    }

    "work for a number with only two digits" in {
      obfuscatePhoneNumber("12") must beEqualTo("12")
    }

    "work for a number with only 1 digit" in {
      obfuscatePhoneNumber("1") must beEqualTo("1")
    }

    "do nothing for a blank phone number" in {
      obfuscatePhoneNumber("") must beEqualTo("")
    }

    "return unknown if the number passed through was unknown" in {
      obfuscatePhoneNumber("unknown") must beEqualTo("unknown")
    }
  }
}