package au.gov.csc.snippet

import au.gov.csc.model.SHelpers
import au.gov.csc.model.scheme.{ Scheme, SchemeDefinition }
import au.gov.csc.model.state.SessionState

class LocalisationTest extends org.specs2.mutable.Specification with StringHelpers with SHelpers {

  var schemeList: Map[String, SchemeDefinition] =
    Map("CSS" -> new Scheme(
      "CSS",
      "CSS",
      "https://css.gov.au/",
      "/img/site_logo_css.svg",
      "https://member.comsuper.gov.au/css_memb_cmnprod/"
    ))

  "Localising a string" should {
    "work for a key found in the snippets file" in {
      inSession({
        SessionState.scheme(None)
        ?("btn-get-started-text") mustNotEqual "btn-get-started-text"
      })
    }

    "not work for a key not found in the snippets file" in {
      inSession({
        SessionState.scheme(None)
        ?("my-invented-key") must beEqualTo("my-invented-key")
      })
    }

    "work for no scheme localisation" in {
      inSession({
        SessionState.scheme(None)
        ?("invalid-membership-number-provided") mustNotEqual "invalid-membership-number-provided"
      })
    }

    "work for css scheme localisation" in {
      inSession({
        SessionState.scheme(Some(schemeList("CSS")))
        ?("invalid-membership-number-provided") mustNotEqual "invalid-membership-number-provided-css"
      })
    }
  }
}