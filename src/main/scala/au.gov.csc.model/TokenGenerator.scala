package au.gov.csc.model

trait TokenGenerator {
  def generateToken: String
}

class StableMockTokenGenerator(token: String) extends TokenGenerator {
  override def generateToken: String = {
    token
  }
}

class NextFuncNameTokenProvider extends TokenGenerator {
  import net.liftweb.util.Helpers._
  override def generateToken: String = {
    nextFuncName
  }
}

class RandomStringTokenProvider(length: Int, permittedChars: Array[Char]) extends TokenGenerator {
  val charLength = permittedChars.length
  def getRandomChar: Char = {
    val index = scala.util.Random.nextInt(charLength)
    permittedChars(index)
  }
  override def generateToken: String = {
    new String(Range(0, length).map(i => getRandomChar).toArray)
  }
}
