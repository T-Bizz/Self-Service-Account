package au.gov.csc.model

import java.util.Date

import scala.xml.{ NodeSeq, Text }
import net.liftweb.util.Helpers._

import au.gov.csc.model.SessionState._

trait FactProvider {

  def getFacts(memberNumber: String): Either[Exception, Member]
  def getAccount(memberNumber: String): Either[Exception, AccountDefinition]
}

class MockFactProvider extends FactProvider {

  val mockMemberProvider = new MockMemberProvider
  val mockAccountProvider = new MockAccountProvider

  def getFacts(memberNumber: String): Either[Exception, Member] =
    mockMemberProvider.getMember(memberNumber)

  def getAccount(memberNumber: String): Either[Exception, AccountDefinition] =
    mockAccountProvider.getAccount(memberNumber)
}

trait FactSet {
  val factSetId: String = nextFuncName
  def getHasChosen: Boolean
  def getChoices: Seq[WorkflowTypeChoice.Value]
  def setChoice(choice: WorkflowTypeChoice.Value)
  def getNextQuestions: Option[QuestionSet]
  def answerQuestions(answers: Seq[Answer])
  def isComplete: Boolean
  def canComplete: Boolean
  def getCurrentEmail: String
  def getCurrentMobileNumber: String
  def getRemainingUnansweredQuestionCount: Int
}

class MemberBackedFactSet(
  member: Member,
  minimumNumberOfCorrectAnswers: Int,
  questionsPerPage: Int
)
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
      QuestionSet(
        QuestionSetType.Hygiene,
        Text(?("hygine-questions-title")),
        List(
          StringQuestion(
            QuestionSetType.Hygiene,
            Text(?("hygine-first-name-question")),
            Text(?("hygine-first-name-help-text")),
            ?("hygine-first-name-placeholder"),
            ?("hygine-first-name-icon"),
            false,
            0,
            member.person.givenNames
          ),
          StringQuestion(
            QuestionSetType.Hygiene,
            Text(?("hygine-surname-question")),
            Text(?("hygine-surname-help-text")),
            ?("hygine-surname-placeholder"),
            ?("hygine-surname-icon"),
            false,
            1,
            member.person.surname
          ),
          NumberQuestion(
            QuestionSetType.Hygiene,
            Text(?("hygine-age-question")),
            Text(?("hygine-age-help-text")),
            ?("hygine-age-placeholder"),
            ?("hygine-age-icon"),
            false,
            2,
            member.person.age.toString
          )
        ) ::: member.person.taxFileNumber.toList.map(t =>
            NumberQuestion(
              QuestionSetType.Hygiene,
              Text(?("hygine-tfn-question")),
              Text(?("hygine-tfn-help-text")),
              ?("hygine-tfn-placeholder"),
              ?("hygine-tfn-icon"),
              false,
              4,
              t
            )),
        1,
        Some(Text(?("hygine-questions-footer")))
      )
    ) ::: member.memberships.toList.map(m => {
        QuestionSet(
          QuestionSetType.CurrentMembership,
          Text(?("membership-questions-title") + " %s".format(m.external_id)),
          List(
            DateQuestion(
              QuestionSetType.CurrentMembership,
              Text(?("membership-join-date-help-text")),
              Text(?("membership-join-date-question")),
              ?("membership-join-date-placeholder"),
              ?("membership-join-date-icon"),
              false,
              2,
              m.joinDate
            ),
            StringQuestion(
              QuestionSetType.CurrentMembership,
              Text(?("membership-status-question")),
              Text(?("membership-status-help-text")),
              ?("membership-status-placeholder"),
              ?("membership-status-icon"),
              false,
              3,
              m.status
            )
          ) ::: m.exitDate.toList.map(ed => {
              DateQuestion(
                QuestionSetType.CurrentMembership,
                Text(?("membership-exit-date-question")),
                Text(?("membership-exit-help-text")),
                ?("membership-exit-date-placeholder"),
                ?("membership-exit-date-icon"),
                false,
                4,
                ed
              )
            }),
          2,
          Some(Text(?("membership-questions-footer")))
        )
      }) ::: (member.contactDetails.toList.flatMap {
        case e: EmailAddress =>
          List(
            QuestionSet(
              QuestionSetType.TokenEmail,
              Text(?("token-email-title")),
              List(
                TokenQuestion(
                  QuestionSetType.TokenEmail,
                  Text(?("token-email-question")),
                  Text(?("token-email-help-text")),
                  ?("token-email-placeholder"),
                  ?("token-email-icon"),
                  true,
                  0,
                  Left(e)
                )
              ),
              0,
              Some(Text(?("token-email-footer")))
            )
          )
        case e: PhoneNumber if e.kind == "mobile" =>
          List(
            QuestionSet(
              QuestionSetType.TokenSMS,
              Text(?("token-sms-title")),
              List(
                TokenQuestion(
                  QuestionSetType.TokenSMS,
                  Text(?("token-sms-question")),
                  Text(?("token-sms-help-text")),
                  ?("token-sms-placeholder"),
                  ?("token-sms-icon"),
                  true,
                  0,
                  Right(e)
                )
              ),
              0,
              Some(Text(?("token-sms-footer")))
            )
          )
        case _ => Nil
      }) ::: List(
        QuestionSet(
          QuestionSetType.ContactDetails,
          Text(?("contact-details-questions-title")),
          member.contactDetails.flatMap {
            case cd: PhoneNumber => {
              List(
                StringQuestion(
                  QuestionSetType.ContactDetails,
                  Text(?("contact-details-phone-number-question")),
                  Text(?("contact-details-phone-number-help-text")),
                  ?("contact-details-phone-number-placeholder"),
                  ?("contact-details-phone-number-icon"),
                  false,
                  0,
                  cd.phoneNumber
                )
              )
            }
            case cd: Address => {
              List(
                StringQuestion(
                  QuestionSetType.ContactDetails,
                  Text(?("contact-details-post-code-question")),
                  Text(?("Provide your postcode for your current address")),
                  ?("contact-details-post-code-placeholder"),
                  ?("contact-details-post-code-icon"),
                  false,
                  0,
                  cd.postCode
                ),
                StringQuestion(
                  QuestionSetType.ContactDetails,
                  Text(?("contact-details-suburb-question")),
                  Text(?("contact-details-suburb-help-text")),
                  ?("contact-details-suburb-placeholder"),
                  ?("contact-details-suburb-icon"),
                  false,
                  0,
                  cd.city
                ),
                StringQuestion(
                  QuestionSetType.ContactDetails,
                  Text(?("contact-details-state-question")),
                  Text(?("contact-details-state-help-text")),
                  ?("contact-details-state-placeholder"),
                  ?("contact-details-state-icon"),
                  false,
                  0,
                  cd.state
                )
              )
            }
            case _ => Nil
          },
          4,
          None
        )
      )
  }

  protected var unansweredQuestions: List[QuestionBase] = questionSets.flatMap(_.questions)
  protected var correctAnswers: Int = 0
  protected var allMandatoryQuestionsCorrect: Boolean = true
  protected var chosenWorkflowType: Option[WorkflowTypeChoice.Value] = None
  protected var hasChosen = false

  def getRemainingUnansweredQuestionCount = unansweredQuestions.length

  def setChoice(choice: WorkflowTypeChoice.Value) = {
    if (hasChosen == false) {
      unansweredQuestions = choice match {
        case WorkflowTypeChoice.EmailAndQuestions => unansweredQuestions.filterNot {
          case p: TokenQuestion if p.target.isRight => true
          case _                                    => false
        }
        case WorkflowTypeChoice.SmsAndQuestions => unansweredQuestions.filterNot {
          case p: TokenQuestion if p.target.isLeft => true
          case _                                   => false
        }
        case WorkflowTypeChoice.QuestionsOnly => unansweredQuestions.filterNot {
          case p: TokenQuestion => true
          case _                => false
        }
      }
      chosenWorkflowType = Some(choice)
      hasChosen = true;
    }
  }

  def getHasChosen: Boolean = {
    hasChosen
  }

  def getChoices: Seq[WorkflowTypeChoice.Value] = {

    var options: Seq[WorkflowTypeChoice.Value] = Nil
    member.contactDetails.foreach {
      case e: EmailAddress if (questionSets.size >= 2) => {
        options = options :+ SmsAndQuestions
      }
      case m: PhoneNumber if ((m.kind.toLowerCase == "mobile") & (questionSets.size >= 2)) => {
        options = options :+ EmailAndQuestions
      }
      case _ => {}
    }

    if (questionSets.size >= 3) {
      options = options :+ QuestionsOnly
    }

    options
  }

  def answerQuestions(answers: Seq[Answer]) = unansweredQuestions = unansweredQuestions.filterNot {
    case q: QuestionBase if answers.exists(a => a.question == q) => {
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
            case QuestionSetType.TokenEmail => chosenWorkflowType match {
              case Some(EmailAndQuestions) => true
              case _                       => false
            }
            case QuestionSetType.TokenSMS => chosenWorkflowType match {
              case Some(SmsAndQuestions) => true
              case _                     => false
            }
            case _ => true
          }
        )
    ).toList.sortWith((a, b) => a.order < b.order).headOption
  }

  override def isComplete: Boolean = {
    (minimumNumberOfCorrectAnswers <= correctAnswers) & allMandatoryQuestionsCorrect
  }

  override def canComplete: Boolean = {
    (minimumNumberOfCorrectAnswers <= (correctAnswers + unansweredQuestions.length)) & allMandatoryQuestionsCorrect
  }

  def getCurrentEmail: String = {
    member.contactDetails.toList.filter(c => c match {
      case p: EmailAddress if p.kind.toLowerCase == "internet" => true
      case _ => false
    }).map(p => p match {
      case p: EmailAddress => p.address
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
