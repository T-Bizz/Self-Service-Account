package au.gov.csc.snippet

import net.liftweb.http.{RequestVar, S}

object Scheme extends RequestVar[Option[Tuple3[String,String,String]]](None)

trait DetectScheme {
  def getScheme:Option[Tuple3[String,String,String]] = {
    Scheme.is.map(a => Some(a)).getOrElse({
      S.param("scheme").map(s => {
        s.toUpperCase match {
          case "CSS" => Scheme(Some(Tuple3("CSS", "https://css.gov.au/", "/img/site_logo_css.png")))
          case "PSS" => Scheme(Some(Tuple3("PSS", "https://pss.gov.au/", "/img/site_logo_pss.png")))
          case "MSBS" => Scheme(Some(Tuple3("MSBS", "https://militarysuper.gov.au/", "/img/site_logo_msbs.png")))
          case "DFRB" => Scheme(Some(Tuple3("DFRDB", "https://dfrdb.gov.au/", "/img/site_logo_dfrdb.png")))
          case "DFRDB" => Scheme(Some(Tuple3("DFRDB", "https://dfrdb.gov.au/", "/img/site_logo_dfrdb.png")))
          case "ADFC" => Scheme(Some(Tuple3("ADFC", "https://adfsuper.gov.au/adf-cover/", "/img/site_logo_adfc.png")))
          case "1922" => Scheme(Some(Tuple3("CSS", "https://css.gov.au/", "/img/site_logo_css.png")))
          case "PNG" => Scheme(Some(Tuple3("CSC", "https://css.gov.au/", "/img/site_logo_css.png")))
          case _ => Scheme(None)
        }
      }).getOrElse(Scheme(None))
    })
  }
}