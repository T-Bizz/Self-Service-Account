package au.gov.csc.model

import au.gov.csc.model.fact._
import au.gov.csc.model.member._
import au.gov.csc.model.question._
import au.gov.csc.model.scheme._
import au.gov.csc.model.state._

import org.specs2._
import java.util.Date
import scala.xml._

class MemberBackedFactSetTest
    extends org.specs2.mutable.Specification with SHelpers {

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

  "memberBackedFactSet" should {
    "accept a member" in {
      inSession({
        val fp = createFactSetFixture(Map("77929555" -> Member(
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
        )))
        val m = fp.getFacts("77929555")
        val fs = new MemberBackedFactSet(m.right.get)
        fs.getRemainingUnansweredQuestionCount must beEqualTo(4)
      })
    }

    "remove an answered question" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person(
            "testSurname",
            "testFirstName",
            new Date(),
            27,
            "testFullName",
            "Male",
            Some("Senor"),
            Some("87654321")
          ),
          Nil,
          Nil
        )))
        val m = fp.getFacts("1")
        val fs = new MemberBackedFactSet(m.right.get)
        fs.setChoice(WorkflowTypeChoice.QuestionsOnly)
        val qs = fs.getNextQuestions.get
        val expectedQuestionsLeft = fs.getRemainingUnansweredQuestionCount - 1
        fs.answerQuestions(List(QuestionAnswer("badAnswer", qs.questions.head)))
        fs.getRemainingUnansweredQuestionCount must beEqualTo(expectedQuestionsLeft)
      })
    }

    "accept a correct answer to a questionSet which requires only 1 answer, and mark as completed successfully" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person(
            "testSurname",
            "testFirstName",
            new Date(),
            27,
            "testFullName",
            "Male",
            Some("Senor"),
            Some("87654321")
          ),
          Nil,
          Nil
        )))
        val m = fp.getFacts("1")
        val fs = new MemberBackedFactSet(m.right.get)
        fs.setChoice(WorkflowTypeChoice.QuestionsOnly)
        SessionState.minimumCorrectAnswers = 1
        SessionState.questionsPerPage = 1
        val isComplete = fs.isComplete
        val questions = fs.getNextQuestions.get
        fs.answerQuestions(List(QuestionAnswer("testFirstName", questions.questions.head)))
        val isComplete2 = fs.isComplete
        (isComplete == false && isComplete2 == true) must beEqualTo(true)
      })
    }

    "not accept a second correct answer to a question already answered incorrectly" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person(
            "testSurname",
            "testFirstName",
            new Date(),
            27,
            "testFullName",
            "Male",
            Some("Senor"),
            Some("87654321")
          ),
          Nil,
          Nil
        )))
        val m = fp.getFacts("1")
        SessionState.minimumCorrectAnswers = 1
        SessionState.questionsPerPage = 1
        val fs = new MemberBackedFactSet(m.right.get)
        val isComplete = fs.isComplete
        val questions = fs.getNextQuestions.get
        fs.answerQuestions(List(QuestionAnswer("badAnswer", questions.questions.head)))
        val isComplete2 = fs.isComplete
        fs.answerQuestions(List(QuestionAnswer("testFirstName", questions.questions.head)))
        val isComplete3 = fs.isComplete
        (isComplete == false && isComplete2 == false && isComplete3 == false) must beEqualTo(true)
      })
    }

    "show two questions when set with a 2 question pagesize" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person(
            "testSurname",
            "testFirstName",
            new Date(),
            27,
            "testFullName",
            "Male",
            Some("Senor"),
            Some("87654321")
          ),
          Nil,
          Nil
        )))
        val m = fp.getFacts("1")
        val fs: FactSet = new MemberBackedFactSet(m.right.get)
        fs.setChoice(WorkflowTypeChoice.QuestionsOnly)
        SessionState.questionsPerPage = 2
        val len: Number = fs.getNextQuestions match {
          case Some(a) => a.questions.length
          case _       => 0
        }
        len must beEqualTo(2)
      })
    }

    "mark as completed when 2 questions have been correctly answered, and 2 correct answers are required" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person(
            "testSurname",
            "testFirstName",
            new Date(),
            27,
            "testFullName",
            "Male",
            Some("Senor"),
            Some("87654321")
          ),
          Nil,
          Nil
        )))
        val fs: FactSet = new MemberBackedFactSet(fp.getFacts("1").right.get)
        fs.setChoice(WorkflowTypeChoice.QuestionsOnly)
        SessionState.minimumCorrectAnswers = 2
        SessionState.questionsPerPage = 2
        fs.getNextQuestions match {
          case Some(a) => a.questions.map(q => {
            q match {
              case s: StringQuestion => fs.answerQuestions(List(QuestionAnswer(s.correctAnswer, q)))
              case n: NumberQuestion => fs.answerQuestions(List(QuestionAnswer(n.correctAnswer, q)))
              case d: DateQuestion   => fs.answerQuestions(List(QuestionAnswer(d.correctAnswer.toString, q)))
              case e: EmailQuestion  => fs.answerQuestions(List(QuestionAnswer(e.correctAnswer, q)))
              case _                 => Nil
            }
          })
          case _ => Nil
        }
        fs.isComplete must beEqualTo(true)
      })
    }

    "not mark as complete, when 2 questions have been answered, and 2 correct answers are required, but the answers were not both correct" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person(
            "testSurname",
            "testFirstName",
            new Date(),
            27,
            "testFullName",
            "Male",
            Some("Senor"),
            Some("87654321")
          ),
          Nil,
          Nil
        )))
        val m = fp.getFacts("1")
        val fs: FactSet = new MemberBackedFactSet(m.right.get)
        fs.setChoice(WorkflowTypeChoice.QuestionsOnly)
        SessionState.minimumCorrectAnswers = 2
        SessionState.questionsPerPage = 2
        val qs = fs.getNextQuestions.get
        List.range(0, qs.questions.length).foreach(i => {
          i match {
            case 0 => fs.answerQuestions(List(QuestionAnswer("testFirstName", qs.questions(i))))
            case _ => fs.answerQuestions(List(QuestionAnswer("basAnswer", qs.questions(i))))
          }
        })
        fs.isComplete must beEqualTo(false)
      })
    }

    "no SMS token option when member has no mobile number" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person(
            "testSurname",
            "testFirstName",
            new Date(),
            27,
            "testFullName",
            "Male",
            Some("Senor"),
            Some("87654321")
          ),
          Nil,
          List(EmailAddress(
            "Internet",
            "blah@blah.com",
            true
          ))
        )))
        val m = fp.getFacts("1")
        SessionState.minimumCorrectAnswers = 2
        SessionState.questionsPerPage = 4
        val fs: FactSet = new MemberBackedFactSet(m.right.get)

        var isNotFinished: Boolean = true
        var isFound: Boolean = false
        while (fs.canComplete & isNotFinished) {
          fs.getNextQuestions match {
            case Some(a) => {
              a.category match {
                case QuestionSetType.TokenSMS => isFound = true
                case _                        => Nil
              }
              a.questions.map(q => {
                q match {
                  case s: StringQuestion => fs.answerQuestions(List(QuestionAnswer(s.correctAnswer, q)))
                  case n: NumberQuestion => fs.answerQuestions(List(QuestionAnswer(n.correctAnswer, q)))
                  case _                 => Nil
                }
              })
            }
            case _ => {
              isNotFinished = false
            }
          }
        }
        isFound must beEqualTo(false)
      })
    }

    "include SMS token option question when member has a mobile number" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person(
            "testSurname",
            "testFirstName",
            new Date(),
            27,
            "testFullName",
            "Male",
            Some("Senor"),
            Some("87654321")
          ),
          Nil,
          List(EmailAddress(
            "Internet",
            "blah@blah.com",
            true
          ))
        )))
        val m = fp.getFacts("1")
        SessionState.minimumCorrectAnswers = 2
        SessionState.questionsPerPage = 4
        val fs: FactSet = new MemberBackedFactSet(m.right.get)

        var isNotFinished: Boolean = true
        var isFound: Boolean = false
        while (fs.canComplete & isNotFinished) {
          fs.getNextQuestions match {
            case Some(a) => {
              a.category match {
                case QuestionSetType.TokenSMS => isFound = true
                case _                        => Nil
              }
              a.questions.map(q => {
                q match {
                  case s: StringQuestion => fs.answerQuestions(List(QuestionAnswer(s.correctAnswer, q)))
                  case n: NumberQuestion => fs.answerQuestions(List(QuestionAnswer(n.correctAnswer, q)))
                  case _                 => Nil
                }
              })
            }
            case _ => {
              isNotFinished = false
            }
          }
        }
        isFound must beEqualTo(false)
      })
    }

    "for a given factSet, a choice should only be allowed to be made once" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person(
            "testSurname",
            "testFirstName",
            new Date(),
            27,
            "testFullName",
            "Male",
            Some("Senor"),
            Some("87654321")
          ),
          Nil,
          List(EmailAddress(
            "Internet",
            "blah@blah.com",
            true
          ))
        )))
        val m = fp.getFacts("1")
        val fs: FactSet = new MemberBackedFactSet(m.right.get)
        fs.setChoice(WorkflowTypeChoice.EmailAndQuestions)
        val firstCount = fs.getRemainingUnansweredQuestionCount
        fs.setChoice(WorkflowTypeChoice.QuestionsOnly)
        val secondCount = fs.getRemainingUnansweredQuestionCount
        firstCount must beEqualTo(secondCount)
      })
    }

    "no Email token option when member has no Email Address" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person(
            "testSurname",
            "testFirstName",
            new Date(),
            27,
            "testFullName",
            "Male",
            Some("Senor"),
            Some("87654321")
          ),
          Nil,
          List(EmailAddress(
            "Internet",
            "blah@blah.com",
            true
          ))
        )))
        val m = fp.getFacts("1")
        SessionState.minimumCorrectAnswers = 2
        SessionState.questionsPerPage = 4
        val fs: FactSet = new MemberBackedFactSet(m.right.get)

        var isNotFinished: Boolean = true
        var isFound: Boolean = false
        while (fs.canComplete & isNotFinished) {
          fs.getNextQuestions match {
            case Some(a) => {
              a.category match {
                case QuestionSetType.TokenEmail => isFound = true
                case _                          => Nil
              }
              a.questions.map(q => {
                q match {
                  case s: StringQuestion => fs.answerQuestions(List(QuestionAnswer(s.correctAnswer, q)))
                  case n: NumberQuestion => fs.answerQuestions(List(QuestionAnswer(n.correctAnswer, q)))
                  case _                 => Nil
                }
              })
            }
            case _ => {
              isNotFinished = false
            }
          }
        }
        isFound must beEqualTo(false)
      })
    }
  }
}
