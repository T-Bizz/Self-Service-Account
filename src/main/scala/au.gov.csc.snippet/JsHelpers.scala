package au.gov.csc.snippet

import au.gov.csc.model.state.Globals
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE.JsRaw

trait JsHelpers {

  protected def escapeJs(input: String): String = {
    input.replace('"', '\'')
  }

  protected def showModalError(title: String, message: String): JsCmd = {
    JsRaw("showModalError($, \"%s\", \"%s\");".format(escapeJs(title), escapeJs(message)))
  }

  protected def showModal(title: String, message: String): JsCmd = {
    JsRaw("showModal($, \"%s\", \"%s\");".format(escapeJs(title), escapeJs(message)))
  }

  protected def focusFirstInputField: JsCmd = {
    JsRaw("focusFirstInput($, \"%s\");".format(Globals.contentAreaId))
  }

  protected def addValidationMarkup(formGroupId: String, isValid: Boolean, error: String, errorPrefix: String): JsCmd = {
    JsRaw("addValidationMarkup($, \"%s\", \"%s\", \"%s\");".format(formGroupId, isValid, errorPrefix + error))
  }

  protected def setCurrentStage(s: String): JsCmd = {
    JsRaw("setStage($, '%s');".format(s))
  }
}