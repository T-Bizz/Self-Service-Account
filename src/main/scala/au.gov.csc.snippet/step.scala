package au.gov.csc.snippet

import net.liftweb.common.Loggable

import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml.ajaxSubmit
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds.SetHtml
import xml.Text

import scala.util.Random

object step extends Loggable {
  var (step: Int, numberOfSteps: Int, numberOfQuestionsPerPage: Int, routeNumber: Int) = (0, 5, 3, 0)
  var serviceNumber: String = ""
  var title: String = ""

  def render = {

    def process(): JsCmd = {
      Thread.sleep(500 + Random.nextInt(3000))
      incrementStep

      SetHtml("step-form", route) &
        JsCmds.Run("jQuery('#li-step-1').removeClass('disabled').removeClass('active').addClass('" + step1state + "')") &
          JsCmds.Run("jQuery('#li-step-2').removeClass('disabled').removeClass('active').addClass('" + step2state + "')") &
            JsCmds.Run("jQuery('#li-step-3').removeClass('disabled').removeClass('active').addClass('" + step3state + "')") &
              SetHtml("header-title", header) &
                SetHtml("footer-title", footer) &
                  SetHtml("verify-step-number", Text(verifyStepTitle))
    }

    "#step-form" #> route &
      "#header-title" #> header &
        "#footer-title" #> footer &
          "#li-step-1 [class]" #> step1state &
            "#li-step-2 [class]" #> step2state &
              "#li-step-3 [class]" #> step3state &
                "type=submit" #> ajaxSubmit("Next", process)
  }

  def verifyStepTitle: String = {
    if (step != 0 & step != numberOfSteps)
      "Step " + verifyCurrentStepInProcess + " of " + verifyStepsInProcess
    else
      ""
  }

  def verifyCurrentStepInProcess: Int = routeNumber match {
    case _ => step
  }

  def verifyStepsInProcess: Int = routeNumber match {
    case _ => 4
  }

  def header: NodeSeq = routeNumber match {
    case _ => step match {
      case 0 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-0-header"></div>
      case 1 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-1-header"></div>
      case 2 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-2-header"></div>
      case 3 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-3-header"></div>
      case 4 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-4-header"></div>
      case 5 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-5-header"></div>
      case _ => <div id="header-title"></div>
    }
  }

  def footer: NodeSeq = routeNumber match {
    case _ => step match {
      case 0 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-0-footer"></div>
      case 1 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-1-footer"></div>
      case 2 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-2-footer"></div>
      case 3 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-3-footer"></div>
      case 4 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-4-footer"></div>
      case 5 => <div data-lift="embed?what=/ajax-text-snippets-hidden/route-0-step-5-footer"></div>
      case _ => <div id="footer-title"></div>
    }
  }

  def step1state: String = {
    if (step == 0)
      "active"
    else
      "disabled"
  }

  def step2state: String = {
    if (step != 0 & step != numberOfSteps)
      "active"
    else
      "disabled"
  }

  def step3state: String = {
    if (step == numberOfSteps)
      "active"
    else
      "disabled"
  }

  def route: NodeSeq = routeNumber match {
    case _ => step match {
                case 0 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-0"></div>
                case 1 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-1"></div>
                case 2 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-2"></div>
                case 3 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-3"></div>
                case 4 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-4"></div>
                case 5 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-5"></div>
                case _ => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-0"></div>
              }
  }

  def questions(xhtml:NodeSeq): NodeSeq = {
    val nodeBuf = new scala.xml.NodeBuffer
    for (i <- 1 to numberOfQuestionsPerPage) {
      nodeBuf ++= nthQuestion(i)
    }
    nodeBuf
  }

  def nthQuestion(i: Int): NodeSeq = {
    i match {
      case _ => <div data-lift="embed?what=/ajax-templates-hidden/question-answer"></div>
    }
  }

  def incrementStep = {
    step += 1
    if (step > numberOfSteps) {
      step = 0
    }
  }
}