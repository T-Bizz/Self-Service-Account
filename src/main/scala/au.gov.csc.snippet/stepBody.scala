package au.gov.csc.snippet

import net.liftweb.common.Loggable
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmd

import scala.xml.NodeSeq
import net.liftweb.util.Helpers._

object stepBody extends Loggable {

  def render = {

    def twoFactorSelected(): JsCmd = {
      step.step = 3
      step.process()
    }

    "#header-title" #> header &
      "#footer-title" #> footer &
        "#btn-other" #> ajaxOnSubmit(step.process) &
          "#btn-phone" #> ajaxOnSubmit(twoFactorSelected) &
            "#btn-email" #> ajaxOnSubmit(twoFactorSelected)
  }

  def header: NodeSeq = step.routeNumber match {
    case _ => step.step match {
      case 0 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-0-header"></div>
      case 1 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-1-header"></div>
      case 2 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-2-header"></div>
      case 3 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-3-header"></div>
      case 4 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-4-header"></div>
      case 5 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-5-header"></div>
      case _ => <div id="header-title"></div>
    }
  }

  def footer: NodeSeq = step.routeNumber match {
    case _ => step.step match {
      case 0 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-0-footer"></div>
      case 1 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-1-footer"></div>
      case 2 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-2-footer"></div>
      case 3 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-3-footer"></div>
      case 4 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-4-footer"></div>
      case 5 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-5-footer"></div>
      case _ => <div id="footer-title"></div>
    }
  }
}