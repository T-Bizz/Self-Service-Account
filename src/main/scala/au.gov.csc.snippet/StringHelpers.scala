package au.gov.csc.snippet

import au.gov.csc.model.state.SessionState
import net.liftweb.common.Logger
import net.liftweb.http.S

trait StringHelpers extends Logger {

  protected def ?(key: String): String = {
    SessionState.scheme.is match {
      case Some(scheme) => {
        S.?("%s%s".format(key.toLowerCase, "-%s".format(scheme.shortCode)).toLowerCase) match {
          case out if out == "%s%s".format(key.toLowerCase, "-%s".format(scheme.shortCode).toLowerCase) => {
            info("Could not find text snippet with key %s. Replacing it with the key %s.".format(out, key))
            S.?(key)
          }
          case out => out
        }
      }
      case None => {
        info("Could not find scheme for text snippet with key %s. Setting to %s".format(key, S.?(key)))
        S.?(key)
      }
    }
  }

  protected def obscure(text: String) = "*" * text.length

  protected def obfuscatePhoneNumber(in: String): String = in match {
    case "unknown" => in
    case i if i.length > 2 => obscure(i.substring(0, i.length - 2)) + i.substring(i.length - 2)
    case i => i
  }

  protected def obfuscateEmailAddress(in: String): String = {
    val shortMailbox = "(.{1,2})".r
    val longMailbox = "(.)(.*)(.)".r
    val validDomain = """^([a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*)$""".r
    val validEmail = """^([a-zA-Z0-9.!#$%&’'*+/=?^_`{|}~-]+)@([a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*)$""".r

    if (in.split("@").toList.size >= 2) {
      (in, in.split("@").toList.tail(0)) match {
        case (validEmail, validDomain) => {
          (in.split("@").toList.head, in.split("@").toList.tail(0)) match {
            case (shortMailbox(all), domain) => s"${obscure(all)}@$domain"
            case (longMailbox(first, middle, last), domain) => s"$first${obscure(middle)}$last@$domain"
            case _ => "*"
          }
        }
        case _ => {
          warn("Email address %s is invalid.".format(in))
          "*"
        }
      }
    } else {
      "unknown"
    }
  }
}