package au.gov.csc.comet

import net.liftweb.http.js.JsCmds._
import net.liftweb.http.CometActor
import net.liftweb.http.js.JsCmds.SetHtml

import scala.xml.{NodeSeq, Text}
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.util.Schedule
/**
  * Created by Mike on 8/09/2016.
  */

case object Start
case object Stop
case object Ping

class cometTest extends CometActor {
  protected var currentResponse:Option[String] = None
  override def render = {
    "#startButton" #> NodeSeq.Empty &
      "#stopButton" #> NodeSeq.Empty &
      "#cometResponse *" #> currentResponse
  }
  override def fixedRender = {
    "#startButton" #> ajaxButton("start",() => {
      this ! Start
      Noop
    }) &
      "#stopButton" #> ajaxButton("stop",() => {
        this ! Stop
        Noop
      }) &
      "#cometResponse *" #> NodeSeq.Empty
  }
  protected var isRunning = false
  override def lowPriority = {
    case Start => {
      if (isRunning == false) {
        isRunning = true
        this ! Ping
      }
    }
    case Stop => {
      isRunning = false
    }
    case Ping => {
      if (isRunning) {
        currentResponse = Some(new java.util.Date().toString)
        currentResponse.map(cp => partialUpdate(SetHtml("cometResponse", Text(cp))))
        Schedule.schedule(this,Ping,5 seconds)
      }
    }
    case _ => {}
  }

}