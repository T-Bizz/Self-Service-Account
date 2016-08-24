package bootstrap.liftweb

import net.liftweb.common.Full
import net.liftweb.http._
import net.liftweb.http.js.JE
import net.liftweb.sitemap.{Menu, SiteMap}

class Boot {

  def boot {
    LiftRules.addToPackages("au.gov.csc")

    // Display loader and prevent form submission while AJAX is still awaiting a response
    LiftRules.ajaxStart = Full( () => LiftRules.jsArtifacts.show("ajax-spinner").cmd &
      JE.JsRaw("$('input[type=\"submit\"]').prop('disabled', true);").cmd)
    LiftRules.ajaxEnd = Full( () => LiftRules.jsArtifacts.hide("ajax-spinner").cmd &
      JE.JsRaw("$('input[type=\"submit\"]').prop('disabled', false);").cmd)

    LiftRules.responseTransformers.append {
      case Customised(resp) => resp
      case resp => resp
    }

    LiftRules.setSiteMap(SiteMap(Menu.i("Account Management") / "index", Menu.i("Account Management") / "alt"))

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set( (r: Req) =>
      new Html5Properties(r.userAgent))
  }
}