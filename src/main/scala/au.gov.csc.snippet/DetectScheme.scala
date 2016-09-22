package au.gov.csc.snippet

import net.liftweb.http.{ RequestVar, S }
import au.gov.csc.model.SessionState.Scheme
import net.liftweb.common.Logger

trait DetectScheme extends Logger {
  def detectScheme: Option[Tuple3[String, String, String]] = {
    val scheme = S.param("scheme").flatMap(s => {
      s.toUpperCase match {
        case "CSS"   => Some(Tuple3("CSS", "https://css.gov.au/", "/img/site_logo_css.svg"))
        case "PSS"   => Some(Tuple3("PSS", "https://pss.gov.au/", "/img/site_logo_pss.svg"))
        case "MSBS"  => Some(Tuple3("MSBS", "https://militarysuper.gov.au/", "/img/site_logo_msbs.svg"))
        case "DFRB"  => Some(Tuple3("DFRDB", "https://dfrdb.gov.au/", "/img/site_logo_dfrdb.svg"))
        case "DFRDB" => Some(Tuple3("DFRDB", "https://dfrdb.gov.au/", "/img/site_logo_dfrdb.svg"))
        case "ADFC"  => Some(Tuple3("ADFC", "https://adfsuper.gov.au/adf-cover/", "/img/site_logo_adfc.svg"))
        case "1922"  => Some(Tuple3("CSS", "https://css.gov.au/", "/img/site_logo_css.svg"))
        case "PNG"   => Some(Tuple3("CSC", "https://css.gov.au/", "/img/site_logo_css.svg"))
        case _       => None
      }
    })
    info("detectScheme returned: %s".format(scheme))
    scheme
  }

  def getScheme: Option[Tuple3[String, String, String]] = {
    val scheme = Scheme.is.map(a => Some(a)).getOrElse({
      Scheme(detectScheme)
      Scheme.is
    })
    info("getScheme returned: %s".format(scheme))
    scheme
  }
}