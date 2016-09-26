package au.gov.csc.comet

import net.liftweb.common._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{ S, CometActor, CometListener, ListenerManager }
import net.liftweb.actor.LiftActor
import net.liftweb.http.js.JsCmds.{ SetHtml }
import net.liftweb.http.js.JsCmd

import scala.xml.{ NodeSeq, Text }
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._

import au.gov.csc.model._
import au.gov.csc.snippet.SinglePageAppView
import au.gov.csc.model.SessionState._

case class TokenMessage(sessionId: String, token: String)

case class NavigationMessage(sessionId: String, dataId: String, redirectPath: Option[String] = None)

object PushActorManager extends LiftActor with ListenerManager {
  override def createUpdate = Nil
  override def lowPriority = {
    case tm @ TokenMessage(sid, t)       => sendListenersMessage(tm)
    case nm @ NavigationMessage(i, t, s) => sendListenersMessage(nm)
    case _                               => {}
  }
}

class PushActor extends CometActor with CometListener with SinglePageAppView with Logger {

  override def registerWith = PushActorManager

  protected var sId: Option[String] = None
  override def render = {
    "#%s *".format(contentAreaId) #> {
      generateCurrentPageNodeSeq
    }
  }

  override protected def localSetup = {
    trace("cometActor starting up: %s (%s) %s".format(this, currentFactSet.is.map(_.factSetId), pageId))
    sId = Some(SessionState.sessionId.is)
    super.localSetup
  }
  override protected def localShutdown = {
    trace("cometActor shutting down: %s (%s) %s".format(this, currentFactSet.is.map(_.factSetId), pageId))
    partialUpdate(RedirectTo("/sessionTerminated"))
    super.localShutdown
  }
  protected def isTokenForMe(tm: TokenMessage): Boolean = {
    currentFactSet.is.map(fs => {
      fs.factSetId == tm.sessionId
    }).getOrElse(false)
  }

  protected def isNavigationForMe(nm: NavigationMessage): Boolean = {
    sId.exists(_ == nm.sessionId) && nm.dataId != pageId
  }

  override def lowPriority = {
    case tm @ TokenMessage(sid, t) if isTokenForMe(tm) => {
      for {
        fs <- currentFactSet.is
        qs <- fs.getNextQuestions
        q <- qs.questions.headOption
        if q.isInstanceOf[TokenQuestion]
      } yield {
        fs.answerQuestions(List(Answer(t, q)))
        val jsCmd: JsCmd = fs.canComplete match {
          case true  => showModal(?("token-received-title"), ?("token-received-successfully")) & SetHtml(contentAreaId, generateCurrentPageNodeSeq)
          case false => showModalError(?("token-received-title"), ?("token-received-error")) & SetHtml(contentAreaId, generateCurrentPageNodeSeq)
        }
        partialUpdate(jsCmd)
      }
    }
    case nm @ NavigationMessage(factSetId, dataId, redirectPath) if isNavigationForMe(nm) => {
      warn("receiving push: %s %s".format(pageId, nm))
      partialUpdate(subUserAction(dataId, redirectPath))
    }
    case _ => {}
  }
}
