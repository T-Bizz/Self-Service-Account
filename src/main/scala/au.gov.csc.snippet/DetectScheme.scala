package au.gov.csc.snippet
import au.gov.csc.model._
import au.gov.csc.model.scheme._
import au.gov.csc.model.state._
import net.liftweb.http.{ RequestVar, S }
import net.liftweb.common.Logger

trait DetectScheme extends Logger {

  def detectScheme: Option[SchemeDefinition] = {
    Globals.schemeList.get(S.param("scheme").openOr("").toString.toUpperCase) match {
      case Some(m) => {
        trace("detectScheme returned: %s".format(m.shortCode))
        Some(m)
      }
      case None => {
        trace("detectScheme returned no scheme for (%s) from list %s".format(S.param("scheme").toString.toUpperCase, Globals.schemeList))
        None
      }
    }
  }

  def getScheme: Option[SchemeDefinition] = {
    val scheme = SessionState.scheme.is.map(a => Some(a)).getOrElse({
      SessionState.scheme(detectScheme)
      SessionState.scheme.is
    })
    trace("getScheme returned: %s".format(scheme))
    scheme
  }
}