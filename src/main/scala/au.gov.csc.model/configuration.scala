package au.gov.csc.model

import au.gov.csc._
import scala.xml._
import java.util.Date
import net.liftweb.common.Logger

object Configuration extends Logger {

  def getConfProperty(name: String): Option[String] = {
    try {
      val result = sys.env(name)
      info("reading sys property: (%s) => (%s)".format(name, result))
      Some(result)
    } catch {
      case t: Throwable =>
        error("exception when reading property: %s => %s\r\n%s".format(name, t.getMessage, t.getStackTrace))
        None
    }
  }

  protected val filePath = getConfProperty("APP_CONFIG_FILE").getOrElse("appConf/configuration.xml")
  protected val xml = scala.xml.XML.load(filePath)
  protected val dateFormat = new java.text.SimpleDateFormat("YYYY-mm-DD")

  protected def readDate(in: Option[String]): Option[Date] = {
    in.flatMap(t => {
      try {
        Some(dateFormat.parse(t))
      } catch {
        case e: Exception => None
      }
    })
  }

  protected def getChildElems(in: NodeSeq): List[Elem] = {
    val elems = in.toList match {
      case List(e: Elem) => e.child.toList.flatMap {
        case elem: Elem => Some(elem)
        case _          => None
      }
      case _ => Nil
    }
    elems
  }

  def getConfiguation: Tuple4[FactProvider, TokenSender, TokenGenerator, globalConstants] = {
    Tuple4(getUserProvider, getTokenSender, getTokenGenerator, getGlobalConstants)
  }

  protected def getGlobalConstants = {
    var gc = new globalConstants
    trace("get globals")
    getChildElems((xml \\ "globals")) match {

      case List(mfp: Elem) if mfp.label == "questions" => {

        trace("get questions")
        for {
          pp <- (mfp \\ "@maximumPerPage").headOption.map(_.text)
          tf <- (mfp \\ "@minimumCorrectTwoFactor").headOption.map(_.text)
          ntf <- (mfp \\ "@minimumCorrectNonTwoFactor").headOption.map(_.text)
        } yield {

          trace("pp %s".format(pp))
          trace("tf %s".format(tf))
          trace("ntf %s".format(ntf))
          gc.questionsPerPage = pp.toInt
          gc.minimumCorrectTwoFactorAnswers = tf.toInt
          gc.minimumCorrectNonTwoFactorAnswers = ntf.toInt
        }
        gc
      }

      case Nil   => throw new Exception("no token provider configured")
      case other => throw new Exception("too many token providers configured")
    }
  }

  protected def getTokenGenerator = getChildElems((xml \\ "tokenProvider")) match {

    case List(smtp: Elem) if smtp.label == "stableMockTokenProvider" => {
      new StableMockTokenGenerator((smtp \\ "@token").headOption.map(_.text).getOrElse({
        throw new Exception("no token provided on stableMockTokenProvider")
      }))
    }

    case List(nfntp: Elem) if nfntp.label == "nextFuncNameTokenProvider" => {
      new NextFuncNameTokenProvider
    }

    case List(rstp: Elem) if rstp.label == "randomStringTokenProvider" => {
      (for {
        len <- (rstp \\ "@length").headOption.map(_.text.toInt)
        permittedChars <- (rstp \\ "@permittedChars").headOption.map(_.text.toArray)
      } yield {
        new RandomStringTokenProvider(len, permittedChars)
      }).getOrElse({
        throw new Exception("randomStringTokenProvider requires both length and permittedChars to be set")
      })
    }

    case Nil   => throw new Exception("no token provider configured")
    case other => throw new Exception("too many token providers configured")
  }

  protected def getTokenSender = getChildElems((xml \\ "tokenSender")) match {
    case List(mts: Elem) if mts.label == "mockTokenSender" => new MockTokenSender
    case Nil => throw new Exception("no token sender configured")
    case other => throw new Exception("too many token senders configured")
  }

  protected def getUserProvider = getChildElems((xml \\ "factProvider")) match {
    case List(mfp: Elem) if mfp.label == "hardcodedMockFactProvider" => new MockFactProvider
    case List(mfp: Elem) if mfp.label == "mockFactProvider" => {

      val members = (mfp \\ "member").flatMap(m => {

        for {

          person <- (m \\ "person").headOption.flatMap(p => {
            for {
              fn <- (p \\ "@firstname").headOption.map(_.text)
              sn <- (p \\ "@surname").headOption.map(_.text)
              fullName <- (p \\ "@fullName").headOption.map(_.text)
              dob <- readDate((p \\ "@dob").headOption.map(_.text))
              age <- (p \\ "@age").headOption.map(_.text.toInt)
              gender <- (p \\ "@gender").headOption.map(_.text)
            } yield {
              Person(sn, fn, dob, age, fullName, gender, (p \\ "@title").headOption.map(_.text), (p \\ "@taxFileNumber").headOption.map(_.text))
            }
          })

          id <- (m \\ "@id").headOption.map(_.text)
          password <- (m \\ "@password").headOption.map(_.text)
          scheme <- (m \\ "@scheme").headOption.map(_.text)
        } yield {

          val memberships = (m \\ "membership").flatMap(m => {
            for {
              extId <- (m \\ "@externalId").headOption.map(_.text)
              scheme <- (m \\ "@scheme").headOption.map(_.text)
              status <- (m \\ "@status").headOption.map(_.text)
              joinDate <- readDate((m \\ "@joinDate").headOption.map(_.text))
            } yield {
              Membership(extId, scheme, status, joinDate, readDate((m \\ "@exitDate").headOption.map(_.text)))
            }
          })

          val contactDetails = ((m \\ "phoneNumber").flatMap(pn => {
            for {
              kind <- (pn \\ "@kind").headOption.map(_.text)
              phoneNumber <- (pn \\ "@phoneNumber").headOption.map(_.text)
              isValid <- (pn \\ "@isValid").headOption.map(_.text.toBoolean)
            } yield {
              val countryCode = (pn \\ "@countryCode").headOption.map(_.text).getOrElse("")
              val areaCode = (pn \\ "@areaCode").headOption.map(_.text).getOrElse("")
              PhoneNumber(kind, countryCode, areaCode, phoneNumber, isValid)
            }
          }).toList ::: (m \\ "emailAddress").flatMap(ea => {

            for {
              kind <- (ea \\ "@kind").headOption.map(_.text)
              address <- (ea \\ "@address").headOption.map(_.text)
              isValid <- (ea \\ "@isValid").headOption.map(_.text.toBoolean)
            } yield {
              EmailAddress(kind, address, isValid)
            }
          }).toList ::: (m \\ "address").flatMap(a => {

            for {
              kind <- (a \\ "@kind").headOption.map(_.text)
              line1 <- (a \\ "@line1").headOption.map(_.text)
              isValid <- (a \\ "@isValid").headOption.map(_.text.toBoolean)
            } yield {
              val line2 = (a \\ "@line2").headOption.map(_.text).getOrElse("")
              val line3 = (a \\ "@line3").headOption.map(_.text).getOrElse("")
              val city = (a \\ "@city").headOption.map(_.text).getOrElse("")
              val state = (a \\ "@state").headOption.map(_.text).getOrElse("")
              val country = (a \\ "@country").headOption.map(_.text).getOrElse("")
              val postCode = (a \\ "@postCode").headOption.map(_.text).getOrElse("")
              Address(kind, line1, line2, line3, city, state, country, postCode, isValid)
            }
          }).toList)

          val member = Member(person, memberships.toList, contactDetails)
          val accountDefinition = AccountDefinition(id, password, scheme)
          (id, member, accountDefinition)
        }
      }).toList

      new DataDrivenMockFactProvider(members)
    }
    case List(unknown) => throw new Exception("unknown fact provider configured")
    case Nil           => throw new Exception("no fact provider configured")
    case other         => throw new Exception("too many fact providers configured")
  }
}