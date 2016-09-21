package au.gov.csc.snippet
/**
  * Created by Tom and Sarah on 6/09/2016.
  */
import org.specs2._
import java.util.Date
import scala.xml._

trait SHelpers {
  import net.liftweb.http._
  import net.liftweb.common._
  import java.io.ByteArrayInputStream

  def generateFakeSession:LiftSession = {
    new LiftSession("test","",Empty)
  }
  def generateFakeReq(
    parsePath:ParsePath = ParsePath(List("testPath","testResource"),"testSuffix",true,false),
    contextPath:Option[String] = None,
    requestType:RequestType = GetRequest,
    contentType:Box[String] = Empty,
    params:Map[String,List[String]] = Map.empty[String,List[String]],
    uploadedFiles:List[FileParamHolder] = Nil,
    body:Box[Array[Byte]] = Empty
  ):Req = {
    new Req(parsePath,contextPath.getOrElse(""),requestType,contentType,null,System.nanoTime,System.nanoTime,false,() => ParamCalcInfo(params.keys.toList,params,uploadedFiles,body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))),Map.empty[String,String])
  }
  def changeParsePathOfReq(req:Req,parsePath:ParsePath):Req = {
    new Req(parsePath,req.contextPath,req.requestType,req.contentType,req.request,req.nanoStart,req.nanoEnd,req.stateless_?,() => ParamCalcInfo(req.paramNames,req.params,req.uploadedFiles,req.body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))),Map.empty[String,String])
  }
  def changeParamsOnReq(req:Req,params:Map[String,List[String]]):Req = {
    new Req(req.path,req.contextPath,req.requestType,req.contentType,req.request,req.nanoStart,req.nanoEnd,req.stateless_?,() => ParamCalcInfo(params.keys.toList,params,req.uploadedFiles,req.body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))),Map.empty[String,String])
  }
  def changePostBodyOfReq(req:Req,body:Box[Array[Byte]]):Req = {
    new Req(req.path,req.contextPath,req.requestType,req.contentType,req.request,req.nanoStart,req.nanoEnd,req.stateless_?,() => ParamCalcInfo(req.paramNames,req.params,req.uploadedFiles,body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))),Map.empty[String,String])
  }
  def changeUploadedFilesOfReq(req:Req,uploadedFiles:List[FileParamHolder]):Req = {
    new Req(req.path,req.contextPath,req.requestType,req.contentType,req.request,req.nanoStart,req.nanoEnd,req.stateless_?,() => ParamCalcInfo(req.paramNames,req.params,uploadedFiles,req.body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))),Map.empty[String,String])
  }
  def changeMethodOfReq(req:Req,newMethod:RequestType):Req = {
    new Req(req.path,req.contextPath,newMethod,req.contentType,req.request,req.nanoStart,req.nanoEnd,req.stateless_?,() => ParamCalcInfo(req.paramNames,req.params,req.uploadedFiles,req.body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))),Map.empty[String,String])
  }
  def inSession[A](action: => A,session:LiftSession = generateFakeSession,req:Req = generateFakeReq()):A = {
    S.init(req,session){
      action
    }
  }
}

class MemberBackedFactSetTest extends org.specs2.mutable.Specification with SHelpers {
  def createFactSetFixture(members:Map[String,Member] = Map("1" -> Member(Person("testSurname",
                                                               "testFirstName",
                                                               new Date(),
                                                               27,
                                                               "testFullName",
                                                               "Male",
                                                               Some("Senor"),
                                                               Some("77929555")),
                                                               Nil,
                                                               Nil))):FactProvider = {
    new MockFactProvider(){
      override val mockMemberProvider = new MockMemberProvider(){
        override val memberFacts = members

      }
    }
  }
  def createTokenQuestionFixture(tokenGenerate:()=>String,onTokenGenerate:String=>Unit):Tuple2[FactSet,TokenQuestion] = {
    (new MemberBackedFactSet(
      createFactSetFixture().getFacts("1").right.get, 1, 1), new TokenQuestion(
        QuestionSetType.TokenEmail, NodeSeq.Empty, NodeSeq.Empty, "", "", false, 0, Left(
          EmailAddress("", "test@test", true))){
      override protected val tokenGenerator = new TokenGenerator(){
        override def generateToken:String = tokenGenerate()
      }
      override protected val tokenSender = new MockTokenSender(){
        override def send(target:Either[EmailAddress,PhoneNumber],token:String,factSet:FactSet):Option[Exception] = {
          onTokenGenerate(token)
          super.send(target,token,factSet)
        }
      }
    })
  }

  "memberBackedFactSet" should {
    "accept a member" in {
      inSession({
        val fp = createFactSetFixture(Map("77929555" -> Member(Person("testSurname",
                                                               "testFirstName",
                                                               new Date(),
                                                               27,
                                                               "testFullName",
                                                               "Male",
                                                               Some("Senor"),
                                                               Some("77929555")),
                                                               Nil,
                                                               Nil)))
        val m = fp.getFacts("77929555")
        val fs = new MemberBackedFactSet(m.right.get,4,2)
        fs.getRemainingUnansweredQuestionCount must beEqualTo(4)
      })
    }

    "do nothing" in {
      1 must beEqualTo(1)
    }

    "remove an answered question" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(Person("testSurname",
                                                        "testFirstName",
                                                        new Date(),
                                                        27,
                                                        "testFullName",
                                                        "Male",
                                                        Some("Senor"),
                                                        Some("87654321")),
                                                        Nil,
                                                        Nil)))
        val m = fp.getFacts("1")
        val fs = new MemberBackedFactSet(m.right.get,4,2)
        fs.setChoice(WorkflowTypeChoice.QuestionsOnly)
        val questions = fs.getNextQuestions.get
        fs.answerQuestions(List(Answer("badAnswer",questions.questions.head)))
        fs.getRemainingUnansweredQuestionCount must beEqualTo(3)
      })
    }
    "accept a correct answer to a questionSet which requires only 1 answer, and mark as completed successfully" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(Person("testSurname",
                                                        "testFirstName",
                                                        new Date(),
                                                        27,
                                                        "testFullName",
                                                        "Male",
                                                        Some("Senor"),
                                                        Some("87654321")),
                                                        Nil,
                                                        Nil)))
        val m = fp.getFacts("1")
        val fs = new MemberBackedFactSet(m.right.get,1,1)
        fs.setChoice(WorkflowTypeChoice.QuestionsOnly)
        val isComplete = fs.isComplete
        val questions = fs.getNextQuestions.get
        fs.answerQuestions(List(Answer("testFirstName",questions.questions.head)))
        val isComplete2 = fs.isComplete
        isComplete == false && isComplete2 == true
      })
    }
    "not accept a second correct answer to a question already answered incorrectly" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(Person("testSurname",
                                                        "testFirstName",
                                                        new Date(),
                                                        27,
                                                        "testFullName",
                                                        "Male",
                                                        Some("Senor"),
                                                        Some("87654321")),
                                                        Nil,
                                                        Nil)))
        val m = fp.getFacts("1")
        val fs = new MemberBackedFactSet(m.right.get,1,1)
        val isComplete = fs.isComplete
        val questions = fs.getNextQuestions.get
        fs.answerQuestions(List(Answer("badAnswer",questions.questions.head)))
        val isComplete2 = fs.isComplete
        fs.answerQuestions(List(Answer("testFirstName",questions.questions.head)))
        val isComplete3 = fs.isComplete
        isComplete == false && isComplete2 == false && isComplete3 == false
      })
    }

    "show two questions when set with a 2 question pagesize" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(Person("testSurname",
                                                        "testFirstName",
                                                        new Date(),
                                                        27,
                                                        "testFullName",
                                                        "Male",
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
      })
    }

    "mark as completed when 2 questions have been correctly answered, and 2 correct answers are required" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(Person("testSurname",
                                                        "testFirstName",
                                                        new Date(),
                                                        27,
                                                        "testFullName",
                                                        "Male",
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
      })
    }

    "not mark as complete, when 2 questions have been answered, and 2 correct answers are required, but the answers were not both correct" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(Person("testSurname",
          "testFirstName",
          new Date(),
          27,
          "testFullName",
          "Male",
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
      })
    }

    "no SMS token option when member has no mobile number" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person("testSurname",
                 "testFirstName",
                 new Date(),
                 27,
                 "testFullName",
                 "Male",
                 Some("Senor"),
                 Some("87654321")),
          Nil,
          List(EmailAddress("Internet",
                            "blah@blah.com",
                            true)
          ))))
        val m = fp.getFacts("1")
        val fs: FactSet = new MemberBackedFactSet(m.right.get,2,4)

        var isNotFinished: Boolean = true
        var isFound: Boolean = false
        while(fs.canComplete & isNotFinished) {
          fs.getNextQuestions match {
            case Some(a) => {a.category match {
                              case QuestionSetType.TokenSMS => isFound = true
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
      })
    }

    "include SMS token option question when member has a mobile number" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person("testSurname",
            "testFirstName",
            new Date(),
            27,
            "testFullName",
            "Male",
            Some("Senor"),
            Some("87654321")),
          Nil,
          List(EmailAddress("Internet",
            "blah@blah.com",
            true)
          ))))
        val m = fp.getFacts("1")
        val fs: FactSet = new MemberBackedFactSet(m.right.get,2,4)

        var isNotFinished: Boolean = true
        var isFound: Boolean = false
        while(fs.canComplete & isNotFinished) {
          fs.getNextQuestions match {
            case Some(a) => {a.category match {
              case QuestionSetType.TokenSMS => isFound = true
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
      })
    }

    "for a given factSet, a choice should only be allowed to be made once" in {
       inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person("testSurname",
                 "testFirstName",
                 new Date(),
                 27,
                 "testFullName",
                 "Male",
                 Some("Senor"),
                 Some("87654321")),
          Nil,
          List(EmailAddress("Internet",
                            "blah@blah.com",
                            true)))))
        val m = fp.getFacts("1")
        val fs: FactSet = new MemberBackedFactSet(m.right.get,2,4)

        fs.setChoice(WorkflowTypeChoice.EmailAndQuestions)
        val firstCount = fs.getRemainingUnansweredQuestionCount
        fs.setChoice(WorkflowTypeChoice.QuestionsOnly)
        val secondCount = fs.getRemainingUnansweredQuestionCount
        firstCount == secondCount
      })
    }

    "no Email token option when member has no Email Address" in {
      inSession({
        val fp = createFactSetFixture(Map("1" -> Member(
          Person("testSurname",
                 "testFirstName",
                 new Date(),
                 27,
                 "testFullName",
                 "Male",
                 Some("Senor"),
                 Some("87654321")),
          Nil,
          List(EmailAddress("Internet",
                            "blah@blah.com",
                            true)))))
        val m = fp.getFacts("1")
        val fs: FactSet = new MemberBackedFactSet(m.right.get,2,4)

        var isNotFinished: Boolean = true
        var isFound: Boolean = false
        while(fs.canComplete & isNotFinished) {
          fs.getNextQuestions match {
            case Some(a) => {a.category match {
              case QuestionSetType.TokenEmail => isFound = true
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
      })
    }

  }
  "TokenQuestion" should {
    "generate a token when asked" in {
      inSession({
        var sentToken = ""
        val (fs,tq) = createTokenQuestionFixture(() => "testToken",(t:String) => sentToken = t)
        val firstState = sentToken
        tq.ask(fs)
        firstState == "" && sentToken == "testToken"
      })
    }
    "fail to check when not asked" in {
      inSession({
        val (fs,tq) = createTokenQuestionFixture(() => "testToken",(t:String) => {})
        !tq.check(Answer("testToken",tq))
      })
    }
    "check successfully against the generated token" in {
      inSession({
        var sentToken = ""
        val (fs,tq) = createTokenQuestionFixture(() => "testToken",(t:String) => sentToken = t)
        tq.ask(fs)
        tq.check(Answer(sentToken,tq))
      })
    }
    "fail check when a previous token is provided to answer" in {
      inSession({
        var sentToken = ""
        var tokenCount = 0
        val (fs,tq) = createTokenQuestionFixture(() => {
          tokenCount += 1
          "testToken_%s".format(tokenCount)
        },(t:String) => sentToken = t)
        tq.ask(fs)
        val firstState = sentToken
        tq.ask(fs)
        val secondState = sentToken
        !tq.check(Answer(firstState,tq)) && tq.check(Answer(secondState,tq))
      })
    }
  }
}
