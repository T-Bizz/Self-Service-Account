package au.gov.csc.comet

import net.liftweb.common._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{ CometActor, CometListener, ListenerManager, S }
import net.liftweb.actor.LiftActor
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.http.js.JsCmd

import scala.xml.{ NodeSeq, Text }
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import au.gov.csc.model.question._
import au.gov.csc.model._
import au.gov.csc.snippet.SinglePageAppView
import au.gov.csc.model.state._
import au.gov.csc.model.fact._
import org.joda.time.DateTime

case class DeviceMessage(sessionId: String, membershipNumber: String)

case class TokenMessage(sessionId: String, token: String)

case class NavigationMessage(sessionId: String, dataId: String, redirectPath: Option[String] = None)

object PushActorManager extends LiftActor with ListenerManager with Logger {
  override def createUpdate = Nil
  override def lowPriority = {
    case tm @ TokenMessage(sid, t) => sendListenersMessage(tm)
    case nm @ NavigationMessage(i, t, s) => sendListenersMessage(nm)
    case dm @ DeviceMessage(i, m) => sendListenersMessage(dm)
    case _ => {}
  }
}

class PushActor extends CometActor with CometListener with SinglePageAppView with Logger {

  override def registerWith = PushActorManager

  protected var sId: Option[String] = None

  override def render = {
    "#%s *".format(Globals.contentAreaId) #> {
      generateCurrentPageNodeSeq
    }
  }

  override protected def localSetup = {
    trace("cometActor starting up: %s (%s) %s".format(this, SessionState.currentFactSet.is.map(_.factSetId), pageId))
    sId = Some(SessionState.sessionId.is)
    super.localSetup
  }

  override protected def localShutdown = {
    trace("cometActor shutting down: %s (%s) %s".format(this, SessionState.currentFactSet.is.map(_.factSetId), pageId))
    partialUpdate(RedirectTo("/sessionTerminated"))
    super.localShutdown
  }

  protected def isTokenForMe(tm: TokenMessage): Boolean = {
    SessionState.currentFactSet.is.map(fs => {
      fs.factSetId == tm.sessionId
    }).getOrElse(false)
  }

  protected def isNavigationForMe(nm: NavigationMessage): Boolean = {
    sId.exists(_ == nm.sessionId) && nm.dataId != pageId
  }

  protected def isDeviceForMe(nm: DeviceMessage): Boolean = {
    !sId.exists(_ == nm.sessionId) && nm.membershipNumber == SessionState.serviceNumber.getOrElse("")
  }

  override def lowPriority = {
    case tm @ TokenMessage(sid, t) if isTokenForMe(tm) => {
      trace("Pushing comet TokenMessage %s".format(tm))
      for {
        fs <- SessionState.currentFactSet.is
        qs <- fs.getNextQuestions
        q <- qs.questions.headOption
        if q.isInstanceOf[TokenQuestion]
      } yield {
        fs.answerQuestions(List(QuestionAnswer(t, q)))
        val jsCmd: JsCmd = fs.canComplete match {
          case true => showModal(?("token-received-title"), ?("token-received-successfully")) & SetHtml(Globals.contentAreaId, generateCurrentPageNodeSeq)
          case false => showModalError(?("token-received-title"), ?("token-received-error")) & SetHtml(Globals.contentAreaId, generateCurrentPageNodeSeq)
        }
        partialUpdate(jsCmd)
      }
    }
    case nm @ NavigationMessage(factSetId, dataId, redirectPath) if isNavigationForMe(nm) => {
      trace("Pushing comet NavigationMessage %s".format(nm))
      partialUpdate(subscribeToNavigationMessage(dataId, redirectPath))
    }
    case dm @ DeviceMessage(sid, membershipNumber) if isDeviceForMe(dm) => {
      trace("Pushing comet DeviceMessage %s".format(dm))
      partialUpdate(subscribeToDeviceMessage)
    }
    case _ => {
      trace("Received unknown comet message")
    }
  }
}
