package au.gov.csc.snippet

import scala.xml.Text
import net.liftweb.http.S
import net.liftweb.sitemap.Loc
import net.liftweb.util.Helpers.strToCssBindPromoter
import scala.xml.NodeSeq

class breadcrumb {

  @scala.annotation.tailrec
  private def intersperse[T](list: List[T], co: T, acc: List[T] = Nil): List[T] = list match {
    case Nil => Nil
    case one :: Nil => (one :: acc).reverse
    case one :: two :: rest => intersperse(two :: rest, co, co :: one :: acc)
  }

  def bootstrap = "*" #> S.location.map(loc =>
    intersperse(
      loc.breadCrumbs.map { loc =>

        val href = loc.createDefaultLink.getOrElse(NodeSeq.Empty)
        val text = loc.linkText.openOr(NodeSeq.Empty)

        if (loc == S.location.openOr(NodeSeq.Empty))
          <li class="active"><h1>{ text }</h1></li>
        else
          <li><a href={ href }>{ text }</a></li>
      },
      Text(""))).openOr(NodeSeq.Empty)

  def generic = "*" #> S.location.map(loc =>
    intersperse(
      loc.breadCrumbs.map { loc =>

        val href = loc.createDefaultLink.getOrElse(NodeSeq.Empty)
        val text = loc.linkText.openOr(NodeSeq.Empty)

        if (loc == S.location.openOr(NodeSeq.Empty))
          { text }
        else

          <a href={ href }>{ text }</a>
      },
      Text(" / "))).openOr(NodeSeq.Empty)
}