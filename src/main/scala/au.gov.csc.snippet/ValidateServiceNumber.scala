package au.gov.csc.snippet

import scala.util.Random
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmd, JsCmds}

object ValidateServiceNumber {

  def render = {
    var serviceNumber: String = ""

    def addValidationMarkup(isTrue: Boolean): JsCmd = {
      if (isTrue) {
        JsCmds.Run("jQuery('#form-group-serviceNumber').removeClass('has-error').addClass('has-success')")
      } else {
        JsCmds.Run("jQuery('#form-group-serviceNumber').removeClass('has-success').addClass('has-error')")
      }
    }

    "input [onchange]" #> SHtml.onEvent( answer =>
      answer match {
        case _ => addValidationMarkup(Random.nextBoolean())
      }
    )
  }
}