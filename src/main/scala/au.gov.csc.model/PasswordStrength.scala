package scala.au.gov.csc.model

import org.mozilla.javascript._
import net.liftweb.util._
import Helpers._
import net.liftweb.actor._
import net.liftweb.common._

class PasswordStrength {
  lazy val ctx = new JsContext(List("""src/main/webapp/js/zxcvbn.min.js"""))
  def checkPasswordStrength(pwd: String): Double = synchronized {
    ctx.run("""zxcvbn("%s",user_inputs=[])""".format(pwd), (s: Scriptable) => s.get("score", s).toString.toDouble)
  }
}

class JsContext(envFiles: Seq[String]) { // this is slow, because the context takes 200+ms to initialize.  It would be faster if we could cache the context, but that requires thread affiinity
  val errorFileName = "<cmd>"
  val files = envFiles.map(filePath => {
    scala.io.Source.fromFile(filePath).getLines.mkString("\n")
  })

  def constructContext = {
    val cx = Context.enter()
    val sc = cx.initStandardObjects()
    files.foreach(lib => {
      cx.evaluateString(sc, lib, errorFileName, 1, null)
    })
    (cx, sc)
  }

  def run[T](jsString: String, withResult: Scriptable => T): T = {
    val start = System.nanoTime()
    val (context, scope) = constructContext
    val resultObjName = nextFuncName
    val result = context.evaluateString(scope, "var %s = ".format(resultObjName) + jsString, errorFileName, 1, null)
    val res = withResult(scope.get(resultObjName, scope).asInstanceOf[Scriptable])
    Context.exit()
    val end = System.nanoTime()
    print("%03dms,".format((end - start) / (1000 * 1000)))
    res
  }
}
