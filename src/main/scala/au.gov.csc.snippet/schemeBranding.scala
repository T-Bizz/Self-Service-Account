package au.gov.csc.snippet

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.xml._

class SchemeBranding extends Logger with DetectScheme {
  def render = {
    trace("req: %s".format(S.request))
    detectScheme.map(s => {
      ".schemeName *" #> Text(s.shortCode) &
        "body [class+]" #> Text("scheme%s".format(s.shortCode.toUpperCase)) &
        ".scheme-site-link [href]" #> Text(s.publicWebsite) &
        ".scheme-site-logo [src]" #> Text(s.logo)
    }).getOrElse({
      trace("redirecting to no scheme because detect scheme failed")
      S.redirectTo("/noSchemeProvided")
    })
  }
}