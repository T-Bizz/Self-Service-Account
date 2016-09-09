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
      val fp = createFixture(Map("1" -> Member(Person("testSurname","testFirstName",new Date(),27,"testFullName",Some("Senor"),Some("87654321")),Nil,Nil)))
      val m = fp.getFacts("1")
      val fs = new MemberBackedFactSet(m.right.get,4,2)
      fs.getRemainingUnansweredQuestionCount must beEqualTo(3)
    }
    "remove an answered question" in {
      val fp = createFixture(Map("1" -> Member(Person("testSurname","testFirstName",new Date(),27,"testFullName",Some("Senor"),Some("87654321")),Nil,Nil)))
      val m = fp.getFacts("1")
      val fs = new MemberBackedFactSet(m.right.get,4,2)
      val questions = fs.getNextQuestions.get
      fs.answerQuestions(List(Answer("badAnswer",questions.questions.head)))
      fs.getRemainingUnansweredQuestionCount must beEqualTo(2)
    }
    "show two questions when set with a 2 question pagesize" in {
      false
    }
    "show one question when set with a 2 question pagesize, but only one question in the set" in {
      false
    }
    "mark as completed when 2 questions have been correctly answered, and 2 correct answers are required" in {
      false
    }
    "not mark as complete, when 2 questions have been answered, and 2 correct answers are required, but the answers were not both correct" in {
      false
    }
  }
}
