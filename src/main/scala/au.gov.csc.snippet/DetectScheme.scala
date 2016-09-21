package au.gov.csc.snippet

import net.liftweb.http.{ RequestVar, S }
import au.gov.csc.model.SessionState.Scheme

trait DetectScheme {
  def getScheme: Option[Tuple3[String, String, String]] = {
    Scheme.is.map(a => Some(a)).getOrElse({
      S.param("scheme").map(s => {
        s.toUpperCase match {
          case "CSS"   => Scheme(Some(Tuple3("CSS", "https://css.gov.au/", "/img/site_logo_css.svg")))
          case "PSS"   => Scheme(Some(Tuple3("PSS", "https://pss.gov.au/", "/img/site_logo_pss.svg")))
          case "MSBS"  => Scheme(Some(Tuple3("MSBS", "https://militarysuper.gov.au/", "/img/site_logo_msbs.svg")))
          case "DFRB"  => Scheme(Some(Tuple3("DFRDB", "https://dfrdb.gov.au/", "/img/site_logo_dfrdb.svg")))
          case "DFRDB" => Scheme(Some(Tuple3("DFRDB", "https://dfrdb.gov.au/", "/img/site_logo_dfrdb.svg")))
          case "ADFC"  => Scheme(Some(Tuple3("ADFC", "https://adfsuper.gov.au/adf-cover/", "/img/site_logo_adfc.svg")))
          case "1922"  => Scheme(Some(Tuple3("CSS", "https://css.gov.au/", "/img/site_logo_css.svg")))
          case "PNG"   => Scheme(Some(Tuple3("CSC", "https://css.gov.au/", "/img/site_logo_css.svg")))
          case _       => Scheme(None)
        }
      }).getOrElse(Scheme(None))
    })
  }
}
