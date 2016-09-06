package au.gov.csc.snippet

import au.gov.csc.SessionState._
import net.liftweb.common.Loggable
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmd
import scala.xml.NodeSeq
import net.liftweb.util.Helpers._

import au.gov.csc.SessionState._

object stepBody extends Loggable {

  val provider = userProvider
  def render = {

    def twoFactorSelected(): JsCmd = {
      currentStep(3)
      skipTwoFactorStep(false)
      step.process()
    }

    def notTwoFactorSelected(): JsCmd = {
      currentStep(1)
      skipTwoFactorStep(true)
      step.process()
      Thread.sleep(500)
    }

    "#header-title" #> header &
      "#footer-title" #> footer &
        "#btn-other" #> ajaxOnSubmit(notTwoFactorSelected) &
          "#btn-phone" #> ajaxOnSubmit(twoFactorSelected) &
            "#btn-email" #> ajaxOnSubmit(twoFactorSelected)
  }

  def header: NodeSeq = routeNumber.is match {
    /*
  Some(currentStep.is).filterNot(cs => cs > 5 || cs < 0).map(sn => {
  <div data-lift={"embed?what/ajax-text-snippets-hidden/route-%s-step-%s-header".format(routeNumber.is,sn)}/>
  }).getOrElse({
  <div id="header-title"/>
  })
  */
    case 0 => currentStep.is match {
      case 0 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-0-header"></div>
      case 1 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-1-header"></div>
      case 2 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-2-header"></div>
      case 3 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-3-header"></div>
      case 4 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-4-header"></div>
      case 5 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-5-header"></div>
      case _ => <div id="header-title"/>
    }
  }

  def footer: NodeSeq = routeNumber.is match {
    case 0 => currentStep.is match {
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