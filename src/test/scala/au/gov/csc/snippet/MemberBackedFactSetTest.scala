package au.gov.csc.snippet
/**
  * Created by Tom and Sarah on 6/09/2016.
  */
import org.specs2._
import java.util.Date

class MemberBackedFactSetTest extends org.specs2.mutable.Specification {
  def createFixture(members:Map[String,Member]):FactProvider = {
    new MockFactProvider(){
      override val mockMemberProvider = new MockMemberProvider(){
        override val memberFacts = members

      }
    }

  }
  "memberBackedFactSet" should {
    "accept a member" in {
      val fp = createFixture(Map("77929555" -> Member(Person("testSurname",
                                                             "testFirstName",
                                                             new Date(),
                                                             27,
                                                             "testFullName",
                                                             Some("Senor"),
                                                             Some("77929555")),
                                                             Nil,
                                                             Nil)))
      val m = fp.getFacts("77929555")
      val fs = new MemberBackedFactSet(m.right.get,4,2)
      fs.getRemainingUnansweredQuestionCount must beEqualTo(5)
    }

    "do nothing" in {
      1 must beEqualTo(1)
    }

    "remove an answered question" in {
      val fp = createFixture(Map("1" -> Member(Person("testSurname",
                                                      "testFirstName",
                                                      new Date(),
                                                      27,
                                                      "testFullName",
                                                      Some("Senor"),
                                                      Some("87654321")),
                                                      Nil,
                                                      Nil)))
      val m = fp.getFacts("1")
      val fs = new MemberBackedFactSet(m.right.get,4,2)
      val questions = fs.getNextQuestions.get
      fs.answerQuestions(List(Answer("badAnswer",questions.questions.head)))
      fs.getRemainingUnansweredQuestionCount must beEqualTo(4)
    }

    "show two questions when set with a 2 question pagesize" in {
      val fp = createFixture(Map("1" -> Member(Person("testSurname",
                                                      "testFirstName",
                                                      new Date(),
                                                      27,
                                                      "testFullName",
                                                      Some("Senor"),
                                                      Some("87654321")),
                                                      Nil,
                                                      Nil)))
      val m = fp.getFacts("1")
      val fs: FactSet = new MemberBackedFactSet(m.right.get,4,2)
      fs.getNextQuestions match{
        case Some(a) if a.questions.length == 2 => true
        case _ => false
      }
    }

    "mark as completed when 2 questions have been correctly answered, and 2 correct answers are required" in {
      val fp = createFixture(Map("1" -> Member(Person("testSurname",
                                                      "testFirstName",
                                                      new Date(),
                                                      27,
                                                      "testFullName",
                                                      Some("Senor"),
                                                      Some("87654321")),
                                                      Nil,
                                                      Nil)))
      val m = fp.getFacts("1")
      val fs: FactSet = new MemberBackedFactSet(m.right.get,2,2)
      fs.getNextQuestions match{
        case Some(a) => a.questions.map(q => {
          q match {
            case s:StringQuestion => fs.answerQuestions(List(Answer(s.correctAnswer,q)))
            case n:NumberQuestion => fs.answerQuestions(List(Answer(n.correctAnswer,q)))
            /*case d:DateQuestion   => fs.answerQuestions(List(Answer(d.correctAnswer,q)))
            case e:EmailQuestion  => fs.answerQuestions(List(Answer(e.correctAnswer,q)))*/
            case _ => Nil
          }
        })
        case _ => Nil
      }
      fs.isComplete
    }

    "not mark as complete, when 2 questions have been answered, and 2 correct answers are required, but the answers were not both correct" in {
      val fp = createFixture(Map("1" -> Member(Person("testSurname",
        "testFirstName",
        new Date(),
        27,
        "testFullName",
        Some("Senor"),
        Some("87654321")),
        Nil,
        Nil)))
      val m = fp.getFacts("1")
      val fs: FactSet = new MemberBackedFactSet(m.right.get,2,2)
      val questions = fs.getNextQuestions.get
      fs.answerQuestions(List(Answer("testFirstName",questions.questions(0))))
      fs.answerQuestions(List(Answer("badAnswer",questions.questions(1))))
      fs.isComplete match{
        case false => true
        case _     => false
      }
    }

    "no SMS token option when member has no mobile number" in {
      val fp = createFixture(Map("1" -> Member(
        Person("testSurname",
               "testFirstName",
               new Date(),
               27,
               "testFullName",
               Some("Senor"),
               Some("87654321")),
        Nil,
        List(EmailAddress("Internet",
                          "blah@blah.com",
                          true,
                          new Date(),
                          new Date(),
                          None)
        ))))
      val m = fp.getFacts("1")
      val fs: FactSet = new MemberBackedFactSet(m.right.get,2,4)

      var isNotFinished: Boolean = true
      var isFound: Boolean = false
      while(fs.canComplete & isNotFinished) {
        fs.getNextQuestions match {
          case Some(a) => {a.category match {
                            case "sendSMSToken" => isFound = true
                            case _ => Nil
                           }
                           a.questions.map(q => {
                             q match {
                               case s:StringQuestion => fs.answerQuestions(List(Answer(s.correctAnswer,q)))
                               case n:NumberQuestion => fs.answerQuestions(List(Answer(n.correctAnswer,q)))
                               case _ => Nil
                             }
                           })
          }
          case _ => {
            isNotFinished = false
          }
        }
      }
      !isFound
    }

    "include SMS token option question when member has a mobile number" in {
      val fp = createFixture(Map("1" -> Member(
        Person("testSurname",
          "testFirstName",
          new Date(),
          27,
          "testFullName",
          Some("Senor"),
          Some("87654321")),
        Nil,
        List(EmailAddress("Internet",
          "blah@blah.com",
          true,
          new Date(),
          new Date(),
          None)
        ))))
      val m = fp.getFacts("1")
      val fs: FactSet = new MemberBackedFactSet(m.right.get,2,4)

      var isNotFinished: Boolean = true
      var isFound: Boolean = false
      while(fs.canComplete & isNotFinished) {
        fs.getNextQuestions match {
          case Some(a) => {a.category match {
            case "sendSMSToken" => isFound = true
            case _ => Nil
          }
            a.questions.map(q => {
              q match {
                case s:StringQuestion => fs.answerQuestions(List(Answer(s.correctAnswer,q)))
                case n:NumberQuestion => fs.answerQuestions(List(Answer(n.correctAnswer,q)))
                case _ => Nil
              }
            })
          }
          case _ => {
            isNotFinished = false
          }
        }
      }
      !isFound
    }

    "no Email token option when member has no Email Address" in {
      val fp = createFixture(Map("1" -> Member(
        Person("testSurname",
               "testFirstName",
               new Date(),
               27,
               "testFullName",
               Some("Senor"),
               Some("87654321")),
        Nil,
        List(EmailAddress("Internet",
                          "blah@blah.com",
                          true,
                          new Date(),
                          new Date(),
                          None)))))
      val m = fp.getFacts("1")
      val fs: FactSet = new MemberBackedFactSet(m.right.get,2,4)

      var isNotFinished: Boolean = true
      var isFound: Boolean = false
      while(fs.canComplete & isNotFinished) {
        fs.getNextQuestions match {
          case Some(a) => {a.category match {
            case "sendEmailToken" => isFound = true
            case _ => Nil
          }
            a.questions.map(q => {
              q match {
                case s:StringQuestion => fs.answerQuestions(List(Answer(s.correctAnswer,q)))
                case n:NumberQuestion => fs.answerQuestions(List(Answer(n.correctAnswer,q)))
                case _ => Nil
              }
            })
          }
          case _ => {
            isNotFinished = false
          }
        }
      }
      !isFound
    }

  }
}
