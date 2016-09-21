package au.gov.csc.snippet

import net.liftweb.http.js.JsCmds._
import net.liftweb.http.CometActor
import net.liftweb.http.js.JsCmds.SetHtml

import scala.xml.{ NodeSeq, Text }
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.util.Schedule
/**
 * Created by Mike on 8/09/2016.
 */
class ajaxTest {
  def render = {
    "#ajaxElement" #> ajaxButton("getTime", () => {
      SetHtml("ajaxResponse", Text(new java.util.Date().toString()))
    })
  }
}