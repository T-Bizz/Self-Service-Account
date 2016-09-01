package au.gov.csc.snippet

import scala.util.Random
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import net.liftweb.http.SHtml._
import net.liftweb.http.js.{JsCmd, JsCmds}

object ValidateServiceNumber {

  var serviceNumber: String = ""

  def render = {

    def addValidationMarkup(isTrue: Boolean): JsCmd = {
      if (isTrue) {
        JsCmds.Run("jQuery('#form-group-serviceNumber').removeClass('has-error').addClass('has-success')")
      } else {
        JsCmds.Run("jQuery('#form-group-serviceNumber').removeClass('has-success').addClass('has-error')")
      }
    }

    "@serviceNumber" #> text(serviceNumber, s => serviceNumber = s) &
      "@serviceNumber [onchange]" #> SHtml.onEvent( answer =>
        answer match {
          case _ => addValidationMarkup(Random.nextBoolean())
        }
    )
  }
}