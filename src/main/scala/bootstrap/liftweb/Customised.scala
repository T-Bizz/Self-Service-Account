package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.{ LiftResponse, PlainTextResponse, S, Templates }

import scala.xml.{ NodeSeq, Text }
import scala.tools.nsc.interpreter.session

object Customised extends Logger {

  val definedPages = 403 :: 404 :: 500 :: Nil
  val codesToTrackErrors = 500 :: Nil

  def unapply(resp: LiftResponse): Option[LiftResponse] =
    definedPages.find(_ == resp.toResponse.code).flatMap(r => toResponse(r, resp))
  def toResponse(status: Int, resp: LiftResponse): Box[LiftResponse] =

    for {
      session <- S.session
      req <- S.request
      template = Templates(status.toString :: Nil).map(t => {
        if (codesToTrackErrors.contains(status)) {
          if (false /*isDebug*/ ) {
            ("#originalReq *" #> Text(resp.toString)).apply(t)
          } else {
            val errorId = "error_%s_%s".format(status, nextFuncName)
            error("exception on http router: %s - %s".format(errorId, resp.toString))
            ("#originalReq *" #> Text(errorId)).apply(t)
          }
        } else {
          ("#originalReq" #> NodeSeq.Empty).apply(t)
        }
      })
      response <- session.processTemplate(template, req, req.path, status)
    } yield {
      response
    }
}
