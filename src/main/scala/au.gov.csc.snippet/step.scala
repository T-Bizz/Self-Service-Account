package au.gov.csc.snippet

import au.gov.csc.SessionState._
import net.liftweb.common.Loggable
import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.{ JsCmd, JsCmds }
import net.liftweb.http.js.JsCmds.SetHtml
import xml.Text
import scala.util.Random

object step extends Loggable {

  def render = {

    "#step-form" #> route &
      "#li-step-1 [class]" #> step1state &
      "#li-step-2 [class]" #> step2state &
      "#li-step-3 [class]" #> step3state &
      "#submit" #> ajaxOnSubmit(process) &
      "#reset" #> ajaxOnSubmit(reset)
  }

  def reset(): JsCmd = {

    currentStep(0)
    numberOfSteps(5)
    numberOfQuestionsPerPage(3)
    routeNumber(0)
    skipTwoFactorStep(true)

    SetHtml("step-form", route) &
      JsCmds.Run("jQuery('#li-step-1').removeClass('disabled').removeClass('active').addClass('" + step1state + "')") &
      JsCmds.Run("jQuery('#li-step-2').removeClass('disabled').removeClass('active').addClass('" + step2state + "')") &
      JsCmds.Run("jQuery('#li-step-3').removeClass('disabled').removeClass('active').addClass('" + step3state + "')") &
      SetHtml("verify-step-number", Text(verifyStepTitle))
  }

  def process(): JsCmd = {
    Thread.sleep(500 + Random.nextInt(2000))
    incrementStep

    if (currentStep.is == 0) {
      reset()
    } else {
      SetHtml("step-form", route) &
        JsCmds.Run("jQuery('#li-step-1').removeClass('disabled').removeClass('active').addClass('" + step1state + "')") &
        JsCmds.Run("jQuery('#li-step-2').removeClass('disabled').removeClass('active').addClass('" + step2state + "')") &
        JsCmds.Run("jQuery('#li-step-3').removeClass('disabled').removeClass('active').addClass('" + step3state + "')") &
        SetHtml("verify-step-number", Text(verifyStepTitle))
    }
  }

  def verifyStepTitle: String = {
    if (currentStep.is != 0 & currentStep.is != numberOfSteps.is)
      "Step " + verifyCurrentStepInProcess + " of " + verifyStepsInProcess
    else
      ""
  }

  def verifyCurrentStepInProcess: Int = routeNumber.is match {
    case 0 => if (skipTwoFactorStep.is) {
      currentStep.is
    } else {
      if (currentStep.is == 4) {
        2
      } else if (currentStep.is == 5) {
        3
      } else {
        0
      }
    }
  }

  def verifyStepsInProcess: Int = routeNumber.is match {
    case 0 => if (skipTwoFactorStep.is) {
      3
    } else {
      2
    }
  }

  def step1state: String = {
    if (currentStep.is == 0)
      "active"
    else
      "disabled"
  }

  def step2state: String = {
    if (currentStep.is != 0 & currentStep.is != numberOfSteps.is)
      "active"
    else
      "disabled"
  }

  def step3state: String = {
    if (currentStep.is == numberOfSteps.is)
      "active"
    else
      "disabled"
  }

  def route: NodeSeq = routeNumber.is match {
    case 0 => currentStep.is match {
      case 0 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-0"></div>
      case 1 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-1"></div>
      case 2 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-2"></div>
      case 3 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-3"></div>
      case 4 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-4"></div>
      case 5 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-5"></div>
      case 6 => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-6"></div>
      case _ => <div data-lift="embed?what=/ajax-templates-hidden/route-0-step-0"></div>
    }
  }

  def questions(xhtml: NodeSeq): NodeSeq = {
    val nodeBuf = new scala.xml.NodeBuffer
    for (i <- 1 to numberOfQuestionsPerPage.is) {
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
    currentStep(currentStep.is + 1)
    if (currentStep.is > numberOfSteps.is) {
      currentStep(0)
    }

    routeNumber.is match {
      case 0 => if (skipTwoFactorStep.is & currentStep.is == 4) {
        currentStep(6)
        if (!skipTwoFactorStep.is & currentStep.is == 2)
          currentStep(4)
      }
    }
  }
}