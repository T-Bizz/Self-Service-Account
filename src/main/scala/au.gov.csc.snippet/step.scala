package au.gov.csc.snippet

import net.liftweb.common.Loggable
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE.{ValById}
import net.liftweb.http.js.JsCmds
import net.liftweb.json.DefaultFormats
import net.liftweb.util.Helpers._
import scala.xml.{NodeSeq}

object step extends Loggable {
  var number: Int = 0
  var numberOfQuestions = 4

  def render = {

    var name = ""

    def process() : JsCmd = SetHtml("result", Text(name))

    "@name" #> text(name, s => name = s) &
      "type=submit" #> ajaxSubmit("Click Me", process)
  }

  // TODO - make this route to the page we want based on state
  def route(xhtml:NodeSeq): NodeSeq = {
    number match {
      case 1 => deincrementStep
        <div data-lift="embed?what=/ajax-templates-hidden/step-1"></div>
      case _ => incrementStep
        <div data-lift="embed?what=/ajax-templates-hidden/step-0"></div>
    }
  }

  def questions(xhtml:NodeSeq): NodeSeq = {
    val nodeBuf = new scala.xml.NodeBuffer
    for (i <- 1 to numberOfQuestions) {
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
    number += 1
  }

  def deincrementStep = {
    number -= 1
  }
}