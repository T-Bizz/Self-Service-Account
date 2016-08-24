package au.gov.csc.snippet

import net.liftweb.common.Loggable
import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml.{ajaxSubmit, text}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetHtml
import scala.util.Random

object step extends Loggable {
  var (step: Int, numberOfSteps: Int, numberOfQuestionsPerPage: Int) = (0, 4, 3)

  def render = {
    var serviceNumber: String = ""

    def process() : JsCmd = {
      Thread.sleep(500 + Random.nextInt(5000))
      incrementStep
      SetHtml("step-form", route)
    }

    "#step-form" #> route &
      "@serviceNumber" #> text(serviceNumber, s => serviceNumber = s) &
        "type=submit" #> ajaxSubmit("Next", process)
  }

  // TODO - make this route to the page we want based on state
  def route: NodeSeq = {
    step match {
      case 4 => <div data-lift="embed?what=/ajax-templates-hidden/step-3"></div>
      case 3 => <div data-lift="embed?what=/ajax-templates-hidden/step-2"></div>
      case 1 | 2 => <div data-lift="embed?what=/ajax-templates-hidden/step-1"></div>
      case _ => <div data-lift="embed?what=/ajax-templates-hidden/step-0"></div>
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