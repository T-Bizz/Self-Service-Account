package au.gov.csc.model

import org.specs2._
import java.util.Date
import scala.xml._

class TokenQuestionTest extends org.specs2.mutable.Specification with SHelpers {

  def createFactSetFixture(members: Map[String, Member] = Map("1" -> Member(
    Person(
      "testSurname",
      "testFirstName",
      new Date(),
      27,
      "testFullName",
      "Male",
      Some("Senor"),
      Some("77929555")
    ),
    Nil,
    Nil
  ))): FactProvider = {
    new MockFactProvider() {
      override val mockMemberProvider = new MockMemberProvider() {
        override val memberFacts = members

      }
    }
  }

  def createTokenQuestionFixture(tokenGenerate: () => String, onTokenGenerate: String => Unit): Tuple2[FactSet, TokenQuestion] = {
    (new MemberBackedFactSet(createFactSetFixture().getFacts("1").right.get), new TokenQuestion(
      QuestionSetType.TokenEmail, NodeSeq.Empty, NodeSeq.Empty, "", "", false, 0, Left(
      EmailAddress("", "test@test", true)
    )
    ) {
      override protected val tokenGenerator = new TokenGenerator() {
        override def generateToken: String = tokenGenerate()
      }
      override protected val tokenSender = new MockTokenSender() {
        override def send(target: Either[EmailAddress, PhoneNumber], token: String, factSet: FactSet): Option[Exception] = {
          onTokenGenerate(token)
          super.send(target, token, factSet)
        }
      }
    })
  }

  "TokenQuestion" should {
    "generate a token when asked" in {
      inSession({
        var sentToken = ""
        val (fs, tq) = createTokenQuestionFixture(() => "testToken", (t: String) => sentToken = t)
        val firstState = sentToken
        tq.ask(fs)
        firstState == "" && sentToken == "testToken"
      })
    }

    "fail to check when not asked" in {
      inSession({
        val (fs, tq) = createTokenQuestionFixture(() => "testToken", (t: String) => {})
        !tq.check(Answer("testToken", tq))
      })
    }

    "check successfully against the generated token" in {
      inSession({
        var sentToken = ""
        val (fs, tq) = createTokenQuestionFixture(() => "testToken", (t: String) => sentToken = t)
        tq.ask(fs)
        tq.check(Answer(sentToken, tq))
      })
    }

    "fail check when a previous token is provided to answer" in {
      inSession({
        var sentToken = ""
        var tokenCount = 0
        val (fs, tq) = createTokenQuestionFixture(() => {
          tokenCount += 1
          "testToken_%s".format(tokenCount)
        }, (t: String) => sentToken = t)
        tq.ask(fs)
        val firstState = sentToken
        tq.ask(fs)
        val secondState = sentToken
        !tq.check(Answer(firstState, tq)) && tq.check(Answer(secondState, tq))
      })
    }
  }
}