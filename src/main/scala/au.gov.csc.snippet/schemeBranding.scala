package au.gov.csc.snippet

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.xml._

class schemeBranding extends Logger with DetectScheme {
  def render = {
    getScheme.map(s => {
      ".schemeName *" #> Text(s._1) &
        "body [class+]" #> Text("scheme%s".format(s._1.toUpperCase)) &
        ".scheme-site-link [href]" #> Text(s._2) &
        ".scheme-site-logo [src]" #> Text(s._3)
    }).getOrElse({
      S.redirectTo("/noSchemeProvided")
    })
  }
}