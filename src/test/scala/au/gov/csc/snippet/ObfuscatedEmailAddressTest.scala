package au.gov.csc.snippet

import org.specs2._

class ObfuscatedEmailAddressTest extends org.specs2.mutable.Specification with SinglePageAppView {

  "Obfuscating an email address" should {
    "work for a long mailbox" in {
      obfuscateEmailAddress("abcdef@example.com") must beEqualTo("a****f@example.com")
    }

    "work for a single letter mailbox" in {
      obfuscateEmailAddress("a@example.com") must beEqualTo("*@example.com")
    }

    "work for a email address with a two letter mailbox" in {
      obfuscateEmailAddress("ab@example.com") must beEqualTo("**@example.com")
    }

    "work for a email address with a three letter mailbox" in {
      obfuscateEmailAddress("abc@example.com") must beEqualTo("a*c@example.com")
    }

    "do nothing for a email address with a three letter mailbox with * in the middle" in {
      obfuscateEmailAddress("a*c@example.com") must beEqualTo("a*c@example.com")
    }
  }
}