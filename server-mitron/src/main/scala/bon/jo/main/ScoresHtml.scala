package bon.jo.main

import java.io.Writer

import bon.jo.controller.Scores
import bon.jo.main.ScoresHtml._
import bon.jo.model.Score
import bon.jo.model.Score.ScoreTest
import Html._

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object HtmlTemplateSys  {

  val HtmlEnv = System.getenv().asScala.iterator.map(e => {
    `<p>`(e._1 + " = " + e._2)
  })

  implicit val noParent: Option[H] = None

  val `<meta/>` = HtmlNode(name = "meta", _atr = List("name" -> "viewport", "content" -> "width=device-width, initial-scale=1, shrink-to-fit=no"))
  val `<css/>` = HtmlNode(name = "link", _atr = List("rel" -> "stylesheet",
    "href" -> "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
    , "integrity" -> "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
    , "crossorigin" -> "anonymous"
  ))


  implicit val keep = mutable.ListBuffer[H]()

  Html

    .start.keep
    .`><head`.`/><body`.`><div`.atr(Map("id" -> "root", "class" -> "container")).`/>`


  //.`/>`
  val htmlObj = keep.head
  println(htmlObj)
  println(htmlObj.hcodeString)
  println(htmlObj.toHTMLString)

  val head = htmlObj.findFirst("head").get
  val div = htmlObj.findFirst("div").get
  head + `<meta/>`
  head + `<css/>`
  div.content = HtmlEnv.to(Iterable)
  def template = htmlObj
}

class HtmlText(override val text: String)(implicit val _parent: Option[H]) extends HtmlNode(text) with H {
  override val name: String = text

  override def toHTMLString: String = text

}

sealed trait H {


  this: HtmlNode =>

  def text: String = ""

  def text_=(text: String): Unit = {
    this + new HtmlText(text)
  }

  def findFirst(value: String): Option[H] = {
    val firstLvel = content.find(_.name == value)
    firstLvel match {
      case Some(_) => firstLvel
      case _ => content.flatMap(_.findFirst(value)).headOption
    }
  }

  def findHead(value: "head"): H = findFirst(value).get

  def keep(implicit keep: mutable.ListBuffer[H]): H = {
    keep += this
    this
  }

  //type Ht = this.type

  def name: String

  var content: Iterable[H]

  def _atr: Iterable[(String, String)]

  var next: Option[H]

  def nextToMe(name: String): H = {
    nextToMe(HtmlNode(name)(parent))
  }

  def nextToMe(el: H): H = {
    println(hcodeString + " next to me : " + el.hcodeString)
    next = Some(el);
    addToChild(parent.get, next.get);
    next.get
  }

  def child(name: String): H = {
    val child = HtmlNode(name)(Some(this));
    child
  }

  def child(childEl: H): Unit = {
    childEl.parent = Some(this)
    addToChild(this, childEl)
  }

  def meToParent: H = {
    println(hcodeString + " me to parent : ")
    println("firstRootChild :  \n" + Html.firstRootChild.hcodeString)
    parent.get.content = parent.get.content.foldRight(List[H](this))(List[H](_) ++ _);
    println("firstRootChild  :  \n" + Html.firstRootChild.hcodeString)
    parent.get
  }

  implicit var parent: Option[H]

  def +(el: H): Unit = {
    this.child(el)
  }

  def `/><div`: H = {
    meToParent;
    this.nextToMe("div")
  }

  def `/><body`: H = {
    meToParent;
    this.nextToMe("body")
  }

  def `/><ul`: H = {
    meToParent;
    this.nextToMe("ul")
  }

  def `/><li`: H = {
    meToParent;
    this.nextToMe("li")
  }

  def `/><p`: H = {
    meToParent;
    this.nextToMe("p")
  }

  def `/><h1`: H = {
    meToParent;
    this.nextToMe("h1")
  }

  def `/><head`: H = {
    meToParent;
    this.nextToMe("head")
  }

  def `/><script`: H = {
    meToParent;
    this.nextToMe("script")
  }

  def `/><meta`: H = {
    meToParent;
    this.nextToMe("meta")
  }

  def `/>`: H = meToParent

  def `><div`: H = child("div")

  def `><head`: H = child("head")

  def `><body`: H = child("body")

  def `><ul`: H = child("ul")

  def `><li`: H = child("li")

  def `><p`: H = child("p")

  def `><h1`: H = child("h1")

  def `<script`: H = child("script")

  def `<meta`: H = child("meta")

  def hcodeString = name + " = " + this.hashCode()

  def atr(at: Map[String, String]): H = this.copy(_atr = at)

  def toHTMLString: String = {
    println(hcodeString)
    s"""<$name${if (_atr.isEmpty) "" else " " + _atr.map(e => s"""${e._1}="${e._2}"""").mkString(" ")}>${content.map(_.toHTMLString).mkString("\n")}</$name>"""

  }
}

case class HtmlNode(name: String, var content: Iterable[H] = Nil, _atr: Iterable[(String, String)] = Nil, var next: Option[H] = None)(implicit var parent: Option[H]) extends H

object Html {

  val root = html_("root")(None)
  var firstRootChild: H = null

  def start: H = {

    firstRootChild = html_("html")(Some(root))
    firstRootChild
  }

  def html_<(name: String)(implicit root: Option[H]): H = {
    val n = HtmlNode(name);
    addToChild(root.get, n);
    n
  }

  def html_(name: String)(implicit parent: Option[H]): H = HtmlNode(name)

  def addToChild(parent: H, child: H): H = {
    parent.content = parent.content.foldRight(List(child))(List(_) ++ _)
    child
  }

  def `<p>`(str: String) = {
    val p =  html_("p")(None)
    p.text = str
    p
  }

}

class ScoresHtml {

}

object ScoresHtml {

  def htmlTemplateSorted(scores: Scores)(implicit t: TemplateParam) = {

    val sored = scores.scores.sorted(toComparbleInt)
    scores.scores = sored
    htmlTemplate(scores)
  }

  //<meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
  val bootStrapMeta: Option[Map[String, String]] = Some(Map("viewport" -> "width=device-width, initial-scale=1, shrink-to-fit=no"))
  val withBootestrap = TemplateParam("Mitron Scores", css = Some(List("""href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous""""))
    , meta = bootStrapMeta)

  def html(body: String)(implicit t: TemplateParam) = {
    s"""<html lang="${t.lg}">
       |<head>
       |  <meta charset="utf-8">
       |
      |${t.htmlTitle}
       |${t.htmlMeta}
       |
      |${t.htmlCss}
       |
      |</head>
       |
      |<body>
       |$body
       |  <script src="js/scripts.js"></script>
       |</body>
       |</html>""".stripMargin
  }

  def htmlTemplate(scores: Scores)(implicit templateParam: TemplateParam) = {
    html(htmlOut(scores))
  }

  def htmlOut(scores: Scores) = {
    s"""<div class="container"><table class="table">
       |
       |
       |  <thead>
       |    <tr>
       |
       |      <th scope="col">#</th>
       |      <th scope="col">Game</th>
       |      <th scope="col">Score</th>
       |      <th scope="col">Player</th>
       |      <th scope="col">Date</th>
       |    </tr>
       |  </thead>
       |
       |
       | <tbody>
       |${htmlIn(scores)}
       |
       | </tbody></table></div>""".stripMargin
  }

  def htmlIn(scores: Scores): String = {

    scores.scores.zipWithIndex.map(e => html(e._1, e._2 + 1)).mkString("\n")
  }

  def html(score: Score, cnt: Int): String = {

    s"""
       | <tr>
       |      <th scope="row">${cnt}</th>
       |      <th scope="row">${score.game}</th>
       |      <td>${score.value}</td>
       |      <td>${score.who.mkString(" ")}</td>
       |      <td>${score.when}</td>
       |    </tr>
      """.stripMargin

  }

  case class TemplateParam(title: String, lg: String = "fr", scrips: Option[List[String]] = None, meta: Option[Map[String, String]] = None, css: Option[List[String]] = None) {
    val htmlTitle = s"<title>$title</title>"
    // val htmllg = s"<title>$title</title>"
    val htmMeta = s"<title>$title</title>"
    val htmlMeta: String = meta match {
      case Some(m) => m.iterator.map(e => s"""  <meta name="${e._1}" content="${e._2}">""").mkString("\n")
      case _ => ""
    }


    val scripsHtml = scrips match {
      case Some(m) => m.map(e => s"""  <script src="$e"></script>""").mkString("\n")
      case _ => ""
    }
    val htmlCss = css match {
      case Some(m) => m.map(e => s"""  <link rel="stylesheet" $e>""").mkString("\n")
      case _ => ""
    }
  }

  def template(scores: Scores)(implicit out: Writer) = {

  }

  def toComparbleInt(s1: Score, s2: Score): Int = {
    if (s1.game == s2.game) {
      if (s1.nbJ == s2.nbJ) {
        if (s1.value == s2.value) {
          if (s1.when == s2.when) {
            if (s1.who == s2.who) {
              0
            } else {
              -s1.who.mkString.compareTo(s2.who.mkString)
            }
          } else {
            -s1.when.compareTo(s2.when)
          }
        } else {
          -s1.value.compareTo(s2.value)
        }
      } else {
        s1.nbJ.compareTo(s2.nbJ)
      }
    } else {
      -s1.game.compareTo(s2.game)
    }
  }


}

object Test extends App {

  implicit val template = withBootestrap
  val s = ScoreTest.randomSeq(100)
  val scores = Scores(s)


  println(htmlTemplateSorted(scores))
}