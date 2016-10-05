package au.gov.csc.snippet
import au.gov.csc.model._
import net.liftweb.http.{ RequestVar, S }
import au.gov.csc.model.SessionState.Scheme
import net.liftweb.common.Logger

trait DetectScheme extends Logger {

  def detectScheme: Option[SchemeDefinition] = {
    Globals.schemeList.get(S.param("scheme").openOr("").toString.toUpperCase) match {
      case Some(m) => {
        trace("detectScheme returned: %s".format(m.shortCode))
        Some(m)
      }
      case None => {
        warn("detectScheme returned no scheme for (%s) from list %s".format(S.param("scheme").toString.toUpperCase, Globals.schemeList))
        None
      }
    }
  }

  def getScheme: Option[SchemeDefinition] = {
    val scheme = Scheme.is.map(a => Some(a)).getOrElse({
      Scheme(detectScheme)
      Scheme.is
    })
    trace("getScheme returned: %s".format(scheme))
    scheme
  }
}