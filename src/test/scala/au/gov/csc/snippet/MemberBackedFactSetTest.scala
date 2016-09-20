package au.gov.csc.snippet
/**
  * Created by Tom and Sarah on 6/09/2016.
  */
import org.specs2._
import java.util.Date

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
  def createFixture(members:Map[String,Member]):FactProvider = {
    new MockFactProvider(){
      override val mockMemberProvider = new MockMemberProvider(){
        override val memberFacts = members

      }
    }

  }
  "memberBackedFactSet" should {
    "accept a member" in {
      inSession({
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
        fs.getRemainingUnansweredQuestionCount must beEqualTo(4)
      })
    }

    "do nothing" in {
      1 must beEqualTo(1)
    }

    "remove an answered question" in {
      inSession({
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
        fs.getRemainingUnansweredQuestionCount must beEqualTo(3)
      })
    }

    "show two questions when set with a 2 question pagesize" in {
      inSession({
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
      })
    }

    "mark as completed when 2 questions have been correctly answered, and 2 correct answers are required" in {
      inSession({
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
      })
    }

    "not mark as complete, when 2 questions have been answered, and 2 correct answers are required, but the answers were not both correct" in {
      inSession({
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
      })
    }

    "no SMS token option when member has no mobile number" in {
      inSession({
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

        fs.setChoice(WorkflowTypeChoice.EmailAndQuestions)
        val firstCount = fs.getRemainingUnansweredQuestionCount
        fs.setChoice(WorkflowTypeChoice.QuestionsOnly)
        val secondCount = fs.getRemainingUnansweredQuestionCount
        firstCount == secondCount
      })
    }

    "no Email token option when member has no Email Address" in {
      inSession({
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
}
