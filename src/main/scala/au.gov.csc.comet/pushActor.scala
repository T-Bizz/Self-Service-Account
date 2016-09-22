package au.gov.csc.comet

import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{ CometActor, CometListener, ListenerManager }
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

class PushActor extends CometActor with CometListener with SinglePageAppView {

  override def registerWith = PushActorManager

  override def render = NodeSeq.Empty

  protected def isTokenForMe(tm: TokenMessage): Boolean = {
    currentFactSet.is.map(fs => {
      fs.factSetId == tm.sessionId
    }).getOrElse(false)
  }

  protected def isNavigationForMe(nm: NavigationMessage): Boolean = {
    currentFactSet.is.map(fs => {
      fs.factSetId == nm.sessionId
    }).getOrElse(false)
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
    case nm @ NavigationMessage(i, t, s) if isNavigationForMe(nm) => {
      for {
        fs <- currentFactSet.is
      } yield {
        partialUpdate(subUserAction(nm.dataId, nm.redirectPath))
      }
    }
    case _ => {}
  }
}
