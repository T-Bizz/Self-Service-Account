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
  var (step: Int, numberOfSteps: Int, numberOfQuestionsPerPage: Int) = (0, 4, 3)
  var serviceNumber: String = ""
  var title : String = ""

  def render = {

    def process() : JsCmd = {
      Thread.sleep(500 + Random.nextInt(3000))
      incrementStep

      SetHtml("step-form", route) &
      JsCmds.Run("jQuery('#li-step-1').removeClass('disabled').removeClass('active').addClass('" + step1state + "')") &
      JsCmds.Run("jQuery('#li-step-2').removeClass('disabled').removeClass('active').addClass('" + step2state + "')") &
      JsCmds.Run("jQuery('#li-step-3').removeClass('disabled').removeClass('active').addClass('" + step3state + "')") &
      SetHtml("heading-title", Text(heading)) &
      SetHtml("verify-step-number", Text(verifyStepTitle))
    }

    "#step-form" #> route &
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


  def verifyCurrentStepInProcess: Int = {
    numberOfSteps match {
      case 3 => step
      case 4 =>
        if (ValidateServiceNumber.serviceNumber == "1")
          1
        else if (ValidateServiceNumber.serviceNumber == "0")
          step
        else
          step
      case 8 => step
    }
  }

  def verifyStepsInProcess: Int = {
    numberOfSteps match {
      case 3 => 2
      case 4 =>
          if (ValidateServiceNumber.serviceNumber == "1")
            1
          else if (ValidateServiceNumber.serviceNumber == "0")
            2
          else
            3
      case 8 => 7
    }
  }

  def heading: String = {

    if (numberOfSteps == 3) {
      if (step == 0) {
        title = "To gain secure access to the complete range of online services, please enter your AGS or Service Number and your email address or mobile phone number below."
      }
      if (step == 1) {
        title = "Some additional details are required to verify your identity. You must provide 5/6 correct answers in order to be issued a PIN."
      }
      if (step == 2) {
        title = "To finish, enter the verification code sent to your nominated device. Contact the Customer Information Center (CIC) if no code is received within the next 5 minutes."
      }
      if (step == 3) {
        title = "Your account has been successfully provisioned."
      }
    }

    if (numberOfSteps == 4) {
      if (step == 0) {
        title = "To gain secure access to the complete range of online services, please enter your AGS or Service Number and your email address or mobile phone number below."
      }
      if (step == 1) {
        title = "Some additional details are required to verify your identity. Please provide answers to the following set of questions about yourself. Set 1 of 2."
      }
      if (step == 2) {
        title = "Some additional details are required to verify your identity. Please provide answers to the following set of questions about your last period of employment. Set 2 of 2."
      }
      if (step == 3) {
        title = "To finish, enter the verification code sent to your nominated device. Contact the Customer Information Center (CIC) if no code is received within the next 5 minutes."
      }
      if (step == 4) {
        title = "Your account has been successfully provisioned."
      }
    }

    if (numberOfSteps == 8) {
      if (step == 0) {
        title = "To gain secure access to the complete range of online services, please enter your AGS or Service Number and your email address or mobile phone number below."
      }
      if (step == 1) {
        title = "Some additional details are required to verify your identity. You must provide 5/6 correct answers in order to be issued a PIN. Click next to skip a question. Question 1 of 6."
      }
      if (step == 2) {
        title = "Some additional details are required to verify your identity. You must provide 5/6 correct answers in order to be issued a PIN. Click next to skip a question. Question 2 of 6."
      }
      if (step == 3) {
        title = "Some additional details are required to verify your identity. You must provide 5/6 correct answers in order to be issued a PIN. Click next to skip a question. Question 3 of 6."
      }
      if (step == 4) {
        title = "Some additional details are required to verify your identity. You must provide 5/6 correct answers in order to be issued a PIN. Click next to skip a question. Question 4 of 6."
      }
      if (step == 5) {
        title = "Some additional details are required to verify your identity. You must provide 5/6 correct answers in order to be issued a PIN. Click next to skip a question. Question 5 of 6."
      }
      if (step == 6) {
        title = "Some additional details are required to verify your identity. You must provide 5/6 correct answers in order to be issued a PIN. Click next to skip a question. Question 6 of 6."
      }
      if (step == 7) {
        title = "To finish, enter the verification code sent to your nominated device. Contact the Customer Information Center (CIC) if no code is received within the next 5 minutes."
      }
      if (step == 8) {
        title = "Your account has been successfully provisioned."
      }
    }

    title
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

  // TODO - make this route to the page we want based on state
  def route: NodeSeq = {
    numberOfQuestionsPerPage match {
      case 3 => step match {
        case 4 => <div data-lift="embed?what=/ajax-templates-hidden/step-3"></div>
        case 3 => <div data-lift="embed?what=/ajax-templates-hidden/step-2"></div>
        case 1 | 2 => <div data-lift="embed?what=/ajax-templates-hidden/step-1"></div>
        case 0 => if (ValidateServiceNumber.serviceNumber == "0")
                    <div data-lift="embed?what=/ajax-templates-hidden/step-0-alt"></div>
                  else
                    <div data-lift="embed?what=/ajax-templates-hidden/step-0"></div>
        case _ => <div data-lift="embed?what=/ajax-templates-hidden/step-0"></div>
      }
      case 6 => step match {
        case 3 => <div data-lift="embed?what=/ajax-templates-hidden/step-3"></div>
        case 2 => <div data-lift="embed?what=/ajax-templates-hidden/step-2"></div>
        case 1 => <div data-lift="embed?what=/ajax-templates-hidden/step-1"></div>
        case 0 => if (ValidateServiceNumber.serviceNumber == "0")
          <div data-lift="embed?what=/ajax-templates-hidden/step-0-alt"></div>
        else
          <div data-lift="embed?what=/ajax-templates-hidden/step-0"></div>
        case _ => <div data-lift="embed?what=/ajax-templates-hidden/step-0"></div>
      }
      case _ => step match {
        case 8 => <div data-lift="embed?what=/ajax-templates-hidden/step-3"></div>
        case 7 => <div data-lift="embed?what=/ajax-templates-hidden/step-2"></div>
        case 1 | 2 | 3 | 4 | 5 | 6 => <div data-lift="embed?what=/ajax-templates-hidden/step-1"></div>
        case 0 => if (ValidateServiceNumber.serviceNumber == "0")
          <div data-lift="embed?what=/ajax-templates-hidden/step-0-alt"></div>
        else
          <div data-lift="embed?what=/ajax-templates-hidden/step-0"></div>
        case _ => <div data-lift="embed?what=/ajax-templates-hidden/step-0"></div>
      }
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

    if (ValidateServiceNumber.serviceNumber == "1" && (step == 1 | step == 2) ) {
      step = 3
    }

    if (ValidateServiceNumber.serviceNumber == "0" && (step == 3) ) {
      step = 4
    }

    if (ValidateServiceNumber.serviceNumber == "0") {
      numberOfQuestionsPerPage = 3
      numberOfSteps = 4
    }

    if (ValidateServiceNumber.serviceNumber == "2") {
      numberOfQuestionsPerPage = 1
      numberOfSteps = 8
    }

    if (ValidateServiceNumber.serviceNumber == "1") {
      numberOfQuestionsPerPage = 3
      numberOfSteps = 4
    }

    if (ValidateServiceNumber.serviceNumber == "3") {
      numberOfQuestionsPerPage = 6
      numberOfSteps = 3
    }

    if (!(ValidateServiceNumber.serviceNumber == "1" | ValidateServiceNumber.serviceNumber == "2" | ValidateServiceNumber.serviceNumber == "3")) {
      numberOfQuestionsPerPage = 3
      numberOfSteps = 4
    }
  }
}