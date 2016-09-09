package au.gov.csc.snippet

import java.util.Date

import scala.xml.{NodeSeq,Text}

trait FactProvider {

  def getFacts(memberNumber: String): Either[Exception, Member]
  def getAccount(memberNumber: String): Either[Exception, AccountDefinition]
}

class MockFactProvider extends FactProvider {

  val mockMemberProvider = new MockMemberProvider
  val mockAccountProvider = new MockAccountProvider

  def getFacts(memberNumber:String):Either[Exception,Member] =
    mockMemberProvider.getMember(memberNumber)

  def getAccount(memberNumber:String):Either[Exception,AccountDefinition] =
    mockAccountProvider.getAccount(memberNumber)
}

  trait FactSet{

    def getHasChosen:Boolean
    def getChoices: Seq[WorkflowTypeChoice.Value]
    def setChoice(choice:WorkflowTypeChoice.Value)
    def getNextQuestions: Option[QuestionSet]
    def answerQuestions(answers:Seq[Answer])
    def isComplete: Boolean
    def canComplete: Boolean
    def getCurrentEmail: String
    def getCurrentMobileNumber: String
  }

  class MemberBackedFactSet(member:Member,
                            minimumNumberOfCorrectAnswers: Int,
                            questionsPerPage: Int)
    extends FactSet {

    import WorkflowTypeChoice._

    protected val questionSets: List[QuestionSet] = {
      List(
      QuestionSet("personal",Text("Questions about yourself"),List(
        StringQuestion("personal", Text("What is your first name?"), NodeSeq.Empty,"John",0,member.person.firstName),
        StringQuestion("personal", Text("What is your surname?"), NodeSeq.Empty,"Smith",1,member.person.surname),
        NumberQuestion("personal", Text("What is your age?"), NodeSeq.Empty,"21",2,member.person.age.toString)
      ) ::: member.person.title.map(t => {
        StringQuestion("personal", Text("What is your title?"), NodeSeq.Empty,"Mr",3,t)
      }).toList ::: member.person.tfn.toList.map(t => {
        NumberQuestion("personal", Text("What is your tax file number?"), NodeSeq.Empty,"87654321",4,t)
      }),1,Some(Text("Click next to skip")))
      ) ::: member.memberships.toList.map(m => {
        val mid = "membership_%s".format(m.membershipNumber)
        QuestionSet(mid,Text("Questions about your membership with ID number %s".format(m.membershipNumber)),List(
          StringQuestion(mid,Text("What is the name of the scheme?"),NodeSeq.Empty,"PSS",0,m.scheme),
          DateQuestion(mid,Text("When did you join this scheme?"),NodeSeq.Empty,"21/6/1985",1,m.joinDate),
          StringQuestion(mid,Text("What is your status?"),NodeSeq.Empty,"contributor",2,m.status)
        ) ::: m.exitDate.toList.map(ed => {
          DateQuestion(mid,Text("When did you exit this scheme?"),NodeSeq.Empty,"21/6/1985",3,ed)
        }),2,Some(Text("Click next to skip")))
      }) ::: (member.contactDetails.toList.flatMap {
        case e: EmailAddress => List(QuestionSet("sendEmailToken", Text("We're sending you a token to your email address"), List(
          TokenQuestion("sendEmailToken", Text("Please enter the token you've received in your email"), NodeSeq.Empty, "012345", 0, Left(e))
        ), 0, None))
        case e: PhoneNumber if e.kind == "mobile" => List(QuestionSet("sendSMSToken", Text("We're sending you a token to your mobile phone"), List(
          TokenQuestion("sendSMSToken", Text("Please enter the token you've received on your phone"), NodeSeq.Empty, "012345", 0, Right(e))
        ), 0, None))
        case _ => Nil
      }) ::: List(QuestionSet("contactDetails",Text("Tell us about how we communicate with you"),member.contactDetails.flatMap{
        case cd:PhoneNumber if cd.kind != "mobile" => {
          List(
            StringQuestion("contactDetails",Text("What's the area code of your phone number?"),NodeSeq.Empty,"03",0,cd.areaCode)
          )
        }
        case cd:ComplexAddress => {
          List(
            StringQuestion("contactDetails",Text("What's your postcode?"),NodeSeq.Empty,"03",0,cd.postCode),
            StringQuestion("contactDetails",Text("What's your suburb?"),NodeSeq.Empty,"03",0,cd.city),
            StringQuestion("contactDetails",Text("What's your state?"),NodeSeq.Empty,"03",0,cd.state)
          )
        }
        case _ => Nil
      },3,None))
   }

    protected var unansweredQuestions: List[QuestionBase] = questionSets.flatMap(_.questions)
    protected var correctAnswers: Int = 0

    def getRemainingUnansweredQuestionCount = unansweredQuestions.length

    protected var hasChosen = false
    def setChoice(choice:WorkflowTypeChoice.Value) = {
      choice match {
        case WorkflowTypeChoice.EmailAndQuestions => unansweredQuestions.filterNot {
          case p: TokenQuestion if p.target.isRight => true
          case _ => false
        }
        case WorkflowTypeChoice.SmsAndQuestions => unansweredQuestions.filterNot {
          case p: TokenQuestion if p.target.isLeft => true
          case _ => false
        }
        case WorkflowTypeChoice.QuestionsOnly => unansweredQuestions.filterNot {
          case p: TokenQuestion => true
          case _ => false
        }
      }
      hasChosen = true;
    }
    def getHasChosen:Boolean = {
        hasChosen
    }
    def getChoices: Seq[WorkflowTypeChoice.Value] = {
      /*
      var hasMobile = false
      var hasEmail = false
      member.contactDetails.foreach{
        case e:EmailAddress => hasEmail = true
        case m:PhoneNumber if m.kind == "mobile" => hasMobile = true
        case _ => {}
      }
      (hasMobile,hasEmail) match {
        case (true,true) => List(QuestionsOnly,SmsAndQuestions,EmailAndQuestions)
        case (false,true) => List(QuestionsOnly,EmailAndQuestions)
        case (true,false) => List(QuestionsOnly,SmsAndQuestions)
        case (false,false) => List(QuestionsOnly)
      }
      */
      QuestionsOnly :: List(SmsAndQuestions).filter(v => member.contactDetails.exists{
        case p:PhoneNumber if p.kind.toLowerCase == "mobile" => true
        case _ => false
      }) ::: List(EmailAndQuestions).filter(e => member.contactDetails.exists{
        case e:EmailAddress => true
        case _ => false
      })
    }

    def answerQuestions(answers: Seq[Answer]) = unansweredQuestions = unansweredQuestions.filterNot{
      case q:QuestionBase if answers.exists(a => a.question == q ) => {
        if (answers.exists(a => a.question == q && q.check(a))) {
          correctAnswers += 1
        }
        true
      }
      case _ => false
    }

    override def getNextQuestions: Option[QuestionSet] = {
      unansweredQuestions.groupBy(_.category).flatMap(kv => kv._2.grouped(questionsPerPage).toList.flatMap(
        qs => questionSets.find(_.category == kv._1).map(
          questionSet => questionSet.copy(questions = qs)
        ))).toList.sortWith((a,b) => a.order < b.order).headOption
    }

    override def isComplete: Boolean = {
      minimumNumberOfCorrectAnswers <= correctAnswers
    }

    override def canComplete: Boolean = {
      minimumNumberOfCorrectAnswers <= (correctAnswers + unansweredQuestions.length)
    }

    def getCurrentEmail: String = {
      member.contactDetails.toList.filter(c => c match {
        case p:EmailAddress if p.kind.toLowerCase == "internet" => true
        case _ => false
      }).map(p => p match {
        case p:EmailAddress => p.emailAddress
      }).headOption.getOrElse("unknown")
    }

    def getCurrentMobileNumber: String = {
      member.contactDetails.toList.filter(c => c match {
        case p:PhoneNumber if p.kind.toLowerCase == "mobile" => true
        case _ => false
      }).map(p => p match {
        case p:PhoneNumber => p.phoneNumber
      }).headOption.getOrElse("unknown")
    }
}