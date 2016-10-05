package au.gov.csc.model

trait SHelpers {
  import net.liftweb.http._
  import net.liftweb.common._
  import java.io.ByteArrayInputStream

  def generateFakeSession: LiftSession = {
    new LiftSession("test", "", Empty)
  }

  def generateFakeReq(
    parsePath: ParsePath = ParsePath(List("testPath", "testResource"), "testSuffix", true, false),
    contextPath: Option[String] = None,
    requestType: RequestType = GetRequest,
    contentType: Box[String] = Empty,
    params: Map[String, List[String]] = Map.empty[String, List[String]],
    uploadedFiles: List[FileParamHolder] = Nil,
    body: Box[Array[Byte]] = Empty
  ): Req = {
    new Req(parsePath, contextPath.getOrElse(""), requestType, contentType, null, System.nanoTime, System.nanoTime, false, () => ParamCalcInfo(params.keys.toList, params, uploadedFiles, body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))), Map.empty[String, String])
  }

  def changeParsePathOfReq(req: Req, parsePath: ParsePath): Req = {
    new Req(parsePath, req.contextPath, req.requestType, req.contentType, req.request, req.nanoStart, req.nanoEnd, req.stateless_?, () => ParamCalcInfo(req.paramNames, req.params, req.uploadedFiles, req.body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))), Map.empty[String, String])
  }

  def changeParamsOnReq(req: Req, params: Map[String, List[String]]): Req = {
    new Req(req.path, req.contextPath, req.requestType, req.contentType, req.request, req.nanoStart, req.nanoEnd, req.stateless_?, () => ParamCalcInfo(params.keys.toList, params, req.uploadedFiles, req.body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))), Map.empty[String, String])
  }

  def changePostBodyOfReq(req: Req, body: Box[Array[Byte]]): Req = {
    new Req(req.path, req.contextPath, req.requestType, req.contentType, req.request, req.nanoStart, req.nanoEnd, req.stateless_?, () => ParamCalcInfo(req.paramNames, req.params, req.uploadedFiles, body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))), Map.empty[String, String])
  }

  def changeUploadedFilesOfReq(req: Req, uploadedFiles: List[FileParamHolder]): Req = {
    new Req(req.path, req.contextPath, req.requestType, req.contentType, req.request, req.nanoStart, req.nanoEnd, req.stateless_?, () => ParamCalcInfo(req.paramNames, req.params, uploadedFiles, req.body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))), Map.empty[String, String])
  }

  def changeMethodOfReq(req: Req, newMethod: RequestType): Req = {
    new Req(req.path, req.contextPath, newMethod, req.contentType, req.request, req.nanoStart, req.nanoEnd, req.stateless_?, () => ParamCalcInfo(req.paramNames, req.params, req.uploadedFiles, req.body.map(b => BodyOrInputStream(new ByteArrayInputStream(b)))), Map.empty[String, String])
  }

  def inSession[A](action: => A, session: LiftSession = generateFakeSession, req: Req = generateFakeReq()): A = {
    S.init(req, session) {
      action
    }
  }
}