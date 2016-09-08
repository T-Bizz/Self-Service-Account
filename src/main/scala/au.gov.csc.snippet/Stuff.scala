package au.gov.csc.snippet

import java.util.Date

import scala.xml.{NodeSeq,Text}

  case class AccountDefinition(memberNumber: String,
                               password: String,
                               scheme: String)

  case class Member(person: Person,
                    memberships: Seq[Membership],
                    contactDetails: Seq[ContactDetail])

  case class Person(surname: String,
                    firstName: String,
                    dob: Date,
                    age: Int,
                    fullname: String,
                    title: Option[String],
                    tfn: Option[String])

  case class Membership(membershipNumber: String,
                        scheme: String,
                        status: String,
                        joinDate: Date,
                        exitDate: Option[Date],
                        effectDate: Option[Date])
  trait ContactDetail

  case class PhoneNumber(kind: String,
                         areaCode: String,
                         phoneNumber: String,
                         isValid: Boolean,
                         effectDate: Date,
                         startDate: Date,
                         endDate: Option[Date])
    extends ContactDetail

  case class EmailAddress(kind: String,
                          emailAddress: String,
                          isValid: Boolean,
                          effectDate: Date,
                          startDate: Date,
                          endDate: Option[Date])
    extends ContactDetail

  case class Address(kind: String,
                     address: String,
                     isValid: Boolean,
                     effectDate: Date,
                     startDate: Date,
                     endDate: Option[Date])
    extends ContactDetail

  case class ComplexAddress(kind: String,
                            streetNumber: Int,
                            city: String,
                            state: String,
                            country: String,
                            postCode: String,
                            isValid: Boolean,
                            effectDate: Date,
                            startDate: Date,
                            endDate: Option[Date])
    extends ContactDetail

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

  trait MemberProvider {

    def getMember(memberNumber:String):Either[Exception,Member]
  }

  class MockMemberProvider extends MemberProvider {

    val MemberNotFoundException = new Exception("member not found")
    val memberFacts = Map(
      "77929555" -> Member(Person("Smith","John",new Date(),21,"John Smith",Some("Mr"),Some("87654321")),Nil,
        List(PhoneNumber("mobile",
          "1",
          "2",
          true,
        new Date(),
    new Date(),
    Some(new Date())),
    EmailAddress("internet?",
      "tom@tom.com",
      true,
      new Date(),
      new Date(),
      Some(new Date()))))
    )

    override def getMember(memberNumber:String):Either[Exception,Member] = memberFacts.get(memberNumber) match {
      case Some(m) => Right(m)
      case None => Left(MemberNotFoundException)
    }
  }

  trait AccountProvider {

    def getAccount(memberNumber:String):Either[Exception,AccountDefinition]
  }

  class MockAccountProvider extends AccountProvider {

    val AccountNotFoundException = new Exception("account not found")
    val accounts = Map(
      "77929555" -> AccountDefinition("77929555","testPassword","testScheme")
    )

    override def getAccount(memberNumber:String): Either[Exception,AccountDefinition] =
      accounts.get(memberNumber) match {
        case Some(a) => Right(a)
        case None => Left(AccountNotFoundException)
      }
  }

  object WorkflowTypeChoice extends Enumeration {

    type WorkflowTypeChoice = Value

    val QuestionsOnly, SmsAndQuestions, EmailAndQuestions = Value
  }

  object StageTypeChoice extends Enumeration {

    type StageTypeChoice = Value

    val Identify, Verify, Result = Value
  }

  class QuestionBase(val category: String,
                     val title: NodeSeq,
                     val helpText: NodeSeq,
                     val placeHolder: String,
                     val order: Int) {

    def getValidationErrors(answer: String): Seq[String] =
      Nil

    def check(answer: Answer):Boolean =
      false
  }

  case class StringQuestion(override val category: String,
                            override val title: NodeSeq,
                            override val helpText: NodeSeq,
                            override val placeHolder: String,
                            override val order: Int,
                            correctAnswer: String)
    extends QuestionBase(category, title, helpText, placeHolder, order) {

    override def getValidationErrors(answer:String): Seq[String] = answer match {
      case s if s.length < 1 => List("answer cannot be empty")
      case other => Nil
    }

    override def check(answer:Answer): Boolean =
      answer.value == correctAnswer
  }

case class NumberQuestion(override val category: String,
                          override val title: NodeSeq,
                          override val helpText: NodeSeq,
                          override val placeHolder: String,
                          override val order: Int,
                          correctAnswer: String)
  extends QuestionBase(category, title, helpText, placeHolder, order) {

  def isNumeric(input: String): Boolean =
    input.forall(_.isDigit)

  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if !isNumeric(s) => List("answer must be numeric")
    case other => Nil
  }

  override def check(answer:Answer):Boolean =
    answer.value == correctAnswer
}

case class EmailQuestion(override val category: String,
                         override val title:NodeSeq,
                         override val helpText:NodeSeq,
                         override val placeHolder:String,
                         override val order:Int,
                         correctAnswer: String)
  extends QuestionBase(category, title, helpText, placeHolder, order) {

  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if s.length < 1 => List("answer cannot be empty")
    case other => Nil
  }

  override def check(answer:Answer):Boolean =
    answer.value == correctAnswer
}

object TokenGenerator {
  import net.liftweb.util.Helpers._
  def generateToken:String = nextFuncName
}
case class TokenQuestion(override val category: String,
                         override val title:NodeSeq,
                         override val helpText:NodeSeq,
                         override val placeHolder:String,
                         override val order:Int,
                         val target:Either[EmailAddress,PhoneNumber])
  extends QuestionBase(category, title, helpText, placeHolder, order) {
  val correctAnswer = TokenGenerator.generateToken
  override def getValidationErrors(answer:String): Seq[String] = answer match {
    case s if s.length < 1 => List("answer cannot be empty")
    case other => Nil
  }

  override def check(answer:Answer):Boolean =
    answer.value == correctAnswer
}


case class DateQuestion(override val category: String,
                        override val title: NodeSeq,
                        override val helpText: NodeSeq,
                        override val placeHolder: String,
                        override val order: Int,
                        correctAnswer: Date)
  extends QuestionBase(category, title, helpText, placeHolder, order){

  val dateFormat = new java.text.SimpleDateFormat("YYYY-mm-DD")

  override def getValidationErrors(answer:String):Seq[String] = try {
    dateFormat.parse(answer)
    Nil
  } catch {
    case e:Exception => List("could not interpret answer as date")
  }

  override def check(answer:Answer):Boolean = try {
    dateFormat.parse(answer.value).getTime() == correctAnswer.getTime()
  } catch {
    case e:Exception => false
  }
}

case class IntQuestion(override val category: String,
                       override val title: NodeSeq,
                       override val helpText: NodeSeq,
                       override val placeHolder: String,
                       override val order: Int,
                       correctAnswer: Int)
  extends QuestionBase(category, title, helpText, placeHolder, order) {

  override def getValidationErrors(answer:String): Seq[String] = try {
    answer.toInt
    Nil
  } catch {
    case e:Exception => List("could not interpret answer as integer")
  }

  override def check(answer:Answer):Boolean = try {
    answer.value.toInt == correctAnswer
  } catch {
    case e:Exception => false
  }
}

case class DoubleQuestion(override val category: String,
                          override val title: NodeSeq,
                          override val helpText: NodeSeq,
                          override val placeHolder: String,
                          override val order: Int,
                          correctAnswer: Double)
  extends QuestionBase(category, title, helpText, placeHolder, order){

  var threshold = 0.01

  def withinThreshold(a:Double,b:Double): Boolean = {
    a - threshold > b || a + threshold < b
  }

  override def getValidationErrors(answer:String): Seq[String] = try {
    answer.toDouble
    Nil
  } catch {
    case e:Exception => List("could not interpret answer as integer")
  }

  override def check(answer:Answer): Boolean = try {
    withinThreshold(answer.value.toDouble,correctAnswer)
  } catch {
    case e:Exception => false
  }
}

  case class Answer(value: String,
                    question: QuestionBase)

  case class QuestionSet(category: String,
                         title: NodeSeq,
                         questions: Seq[QuestionBase],
                         order: Int,
                         footer: Option[NodeSeq])

  trait FactSet{

    def getHasChosen:Boolean
    def getChoices: Seq[WorkflowTypeChoice.Value]
    def setChoice(choice:WorkflowTypeChoice.Value)
    def getNextQuestions: Option[QuestionSet]
    def answerQuestions(answers:Seq[Answer])
    def isComplete: Boolean
    def canComplete: Boolean
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
      }),0,Some(Text("Click next to skip")))
      ) ::: member.memberships.toList.map(m => {
        val mid = "membership_%s".format(m.membershipNumber)
        QuestionSet(mid,Text("Questions about your membership with ID number %s".format(m.membershipNumber)),List(
          StringQuestion(mid,Text("What is the name of the scheme?"),NodeSeq.Empty,"PSS",0,m.scheme),
          DateQuestion(mid,Text("When did you join this scheme?"),NodeSeq.Empty,"21/6/1985",1,m.joinDate),
          StringQuestion(mid,Text("What is your status?"),NodeSeq.Empty,"contributor",2,m.status)
        ) ::: m.exitDate.toList.map(ed => {
          DateQuestion(mid,Text("When did you exit this scheme?"),NodeSeq.Empty,"21/6/1985",3,ed)
        }),1,Some(Text("Click next to skip")))
      }) ::: (member.contactDetails.toList.flatMap {
        case e: EmailAddress => List(QuestionSet("sendEmailToken", Text("We're sending you a token to your email address"), List(
          TokenQuestion("sendEmailToken", Text("Please enter the token you've received in your email"), NodeSeq.Empty, "012345", 0, Left(e))
        ), 6, None))
        case e: PhoneNumber if e.kind == "mobile" => List(QuestionSet("sendSMSToken", Text("We're sending you a token to your mobile phone"), List(
          TokenQuestion("sendSMSToken", Text("Please enter the token you've received on your phone"), NodeSeq.Empty, "012345", 0, Right(e))
        ), 6, None))
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
      },4,None))
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
        case p:PhoneNumber if p.kind == "mobile" => true
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
      unansweredQuestions.groupBy(_.category).flatMap(kv => kv._2.grouped(questionsPerPage).toList.flatMap(qs => questionSets.find(_.category == kv._1).map(questionSet => questionSet.copy(questions = qs)))).toList.sortWith((a,b) => a.order < b.order).headOption
    }

    override def isComplete: Boolean = {
      minimumNumberOfCorrectAnswers <= correctAnswers
    }

    override def canComplete: Boolean = {
      minimumNumberOfCorrectAnswers <= (correctAnswers + unansweredQuestions.length)
    }
}