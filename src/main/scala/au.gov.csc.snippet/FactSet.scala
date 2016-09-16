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

  import net.liftweb.http._
  import WorkflowTypeChoice._

  protected def ?(key: String): String = {
    // get configured string for scheme or use the default configured string
    var out = S ? "%s%s".format(key, Scheme.is.map(s => "-%s".format(s._1)).getOrElse(""))
    if (out == "%s%s".format(key, Scheme.is.map(s => "-%s".format(s._1)).getOrElse("")))
      out = S ? key
    out
  }

  protected val questionSets: List[QuestionSet] = {
    List(
      QuestionSet("personal",Text(?("hygine-questions-title")),
        List(
          StringQuestion("personal",
                         Text(?("hygine-first-name-question")),
                         Text(?("hygine-first-name-help-text")),
                         ?("hygine-first-name-placeholder"),
                         false,
                         0,
                         member.person.firstName),
          StringQuestion("personal",
                         Text(?("hygine-surname-question")),
                         Text(?("hygine-surname-help-text")),
                         ?("hygine-surname-placeholder"),
                         false,
                         1,
                         member.person.surname),
          NumberQuestion("personal",
                         Text(?("hygine-age-question")),
                         Text(?("hygine-age-help-text")),
                         ?("hygine-age-placeholder"),
                         false,
                         2,
                         member.person.age.toString)
        )
        ::: member.person.tfn.toList.map(t =>
          NumberQuestion("personal",
                         Text(?("hygine-tfn-question")),
                         Text(?("hygine-tfn-help-text")),
                         ?("hygine-tfn-placeholder"),
                         false,
                         4,
                         t)
      ),
      1,
      Some(Text(?("hygine-questions-footer"))))
    ) ::: member.memberships.toList.map(m => {
      val mid = "membership_%s".format(m.membershipNumber)

      QuestionSet(mid,
                  Text("Questions about your membership with ID number %s".format(m.membershipNumber)),
                  List(StringQuestion(mid,
                                      Text("What is the name of the scheme?"),
                                      Text("Provide the name of your scheme"),
                                      "PSS",
                                      false,
                                      1,
                                      m.scheme),
                       DateQuestion  (mid,
                                      Text("When did you join this scheme?"),
                                      Text("Provide the date you entered this scheme"),
                                      "21/6/1985",
                                      false,
                                      2,
                                      m.joinDate),
                       StringQuestion(mid,
                                      Text("What is your status?"),
                                      Text("Provide your scheme status"),
                                      "contributor",
                                      false,
                                      3,
                                      m.status)
                  ) ::: m.exitDate.toList.map(ed => {
                    DateQuestion(mid,
                                 Text("When did you exit this scheme?"),
                                 Text("Provide the date you exited this scheme"),
                                 "21/6/1985",
                                 false,
                                 4,
                                 ed)
                  }),
      2,
      Some(Text("Click next to submit your answers")))
    }) ::: (member.contactDetails.toList.flatMap {
      case e: EmailAddress =>
        List(QuestionSet("sendEmailToken",
                         Text(?("token-email-title")),
                         List(
                           TokenQuestion("sendEmailToken",
                                         Text(?("token-email-question")),
                                         Text(?("token-email-help-text")),
                                         ?("token-email-placeholder"),
                                         true,
                                         0,
                                         Left(e))
                         ),
                         0,
                         Some(Text("token-email-footer"))))
      case e: PhoneNumber if e.kind == "mobile" =>
        List(QuestionSet("sendSMSToken",
                         Text(?("token-sms-title")),
                         List(
                           TokenQuestion("sendSMSToken",
                                         Text(?("token-sms-question")),
                                         Text(?("token-sms-help-text")),
                                         ?("token-sms-placeholder"),
                                         true,
                                         0,
                                         Right(e))
                         ),
                         0,
                         Some(Text(?("token-sms-footer")))))
      case _ => Nil
    }) ::: List(QuestionSet("contactDetails", Text("Tell us about how we communicate with you"), member.contactDetails.flatMap{
      case cd:PhoneNumber if cd.kind != "mobile" => {
        List(
          StringQuestion("contactDetails", Text("What is the area code of your phone number?"), Text("Provide the area code of your contact number"), "03", false, 0, cd.areaCode)
        )
      }
      case cd:ComplexAddress => {
        List(
          StringQuestion("contactDetails", Text("What's your postcode?"), Text("Provide the post code for your current address"), "03", false, 0, cd.postCode),
          StringQuestion("contactDetails", Text("What's your suburb?"),   Text("Provide the suburb for your current address"),    "03", false, 0, cd.city),
          StringQuestion("contactDetails", Text("What's your state?"),    Text("Provide the state of your current address"),      "03", false, 0, cd.state)
        )
      }
      case _ => Nil
    }, 4, None))
  }

  protected var unansweredQuestions: List[QuestionBase] = questionSets.flatMap(_.questions)
  protected var correctAnswers: Int = 0
  protected var allMandatoryQuestionsCorrect: Boolean = true
  protected var chosenWorkflowType: Option[WorkflowTypeChoice.Value] = None
  protected var hasChosen = false

  def getRemainingUnansweredQuestionCount = unansweredQuestions.length

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
    chosenWorkflowType = Some(choice)
    hasChosen = true;
  }

  def getHasChosen:Boolean = {
      hasChosen
  }

  def getChoices: Seq[WorkflowTypeChoice.Value] = {

    var options: Seq[WorkflowTypeChoice.Value] = Nil
    member.contactDetails.foreach{
      case e:EmailAddress if (questionSets.size >= 2) => {
        options = options :+ SmsAndQuestions
      }
      case m:PhoneNumber if ((m.kind.toLowerCase == "mobile") & (questionSets.size >= 2)) => {
        options = options :+ EmailAndQuestions
      }
      case _ => {}
    }

    if (questionSets.size >= 3) {
      options = options :+ QuestionsOnly
    }

    options
  }

  def answerQuestions(answers: Seq[Answer]) = unansweredQuestions = unansweredQuestions.filterNot{
    case q:QuestionBase if answers.exists(a => a.question == q) => {
      if (answers.exists(a => a.question == q && q.check(a))) {
        correctAnswers += 1
      } else if (q.mustBeCorrect) {
        allMandatoryQuestionsCorrect = false
      }
      true
    }
    case _ => false
  }

  override def getNextQuestions: Option[QuestionSet] = {
    unansweredQuestions.groupBy(_.category).flatMap(
      kv => kv._2.grouped(questionsPerPage).toList.flatMap(
        qs => questionSets.find(_.category == kv._1).map(questionSet => questionSet.copy(questions = qs))
    ).filter(
      qs => qs.category match {
        case "sendEmailToken" => chosenWorkflowType match
        {
          case Some(EmailAndQuestions) => true
          case _ => false
        }
        case "sendSMSToken" => chosenWorkflowType match
        {
          case Some(SmsAndQuestions) => true
          case _ => false
        }
        case _ => true
      }
    )).toList.sortWith((a,b) => a.order < b.order).headOption
  }

  override def isComplete: Boolean = {
    (minimumNumberOfCorrectAnswers <= correctAnswers) & allMandatoryQuestionsCorrect
  }

  override def canComplete: Boolean = {
    (minimumNumberOfCorrectAnswers <= (correctAnswers + unansweredQuestions.length)) & allMandatoryQuestionsCorrect
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
      case p: PhoneNumber if p.kind.toLowerCase == "mobile" => true
      case _ => false
    }).map(p => p match {
      case p: PhoneNumber => p.phoneNumber
    }).headOption.getOrElse("unknown")
  }
}