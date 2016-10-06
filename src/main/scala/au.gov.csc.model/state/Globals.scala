package au.gov.csc.model.state

import au.gov.csc.model._
import au.gov.csc.model.fact._
import au.gov.csc.model.scheme._
import au.gov.csc.model.state._

object Globals {
  var userProvider: FactProvider = new MockFactProvider()
  var tokenSender: TokenSender = new MockTokenSender()
  var tokenGenerator: TokenGenerator = new NextFuncNameTokenProvider()
  var constants: GlobalConstants = new GlobalConstants()
  var schemeList: Map[String, SchemeDefinition] = Map()

  def init(in: Tuple5[FactProvider, TokenSender, TokenGenerator, GlobalConstants, Map[String, SchemeDefinition]]) = {
    userProvider = in._1
    tokenSender = in._2
    tokenGenerator = in._3
    constants = in._4
    schemeList = in._5
  }
}