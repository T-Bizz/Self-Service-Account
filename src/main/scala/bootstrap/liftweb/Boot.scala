package bootstrap.liftweb

import net.liftweb.http._
import net.liftweb.sitemap.{Menu, SiteMap}

class Boot {

  def boot {
    LiftRules.addToPackages("au.gov.csc")

    LiftRules.responseTransformers.append {
      case Customised(resp) => resp
      case resp => resp
    }

    LiftRules.setSiteMap(SiteMap(Menu.i("Account Management") / "index", Menu.i("Account Management") / "alt"))

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set( (r: Req) =>
      new Html5Properties(r.userAgent) )
  }
}