package bon.jo.main

import java.io.Writer

import bon.jo.controller.Scores
import bon.jo.main.ScoresHtml._
import bon.jo.model.Score
import bon.jo.model.Score.ScoreTest

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

    scores.scores.zipWithIndex.map( e=> html(e._1,e._2+1)).mkString("\n")
  }

  def html(score: Score,cnt : Int): String = {

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
              -  s1.who.mkString.compareTo(s2.who.mkString)
            }
          } else {
            -  s1.when.compareTo(s2.when)
          }
        } else {
          - s1.value.compareTo(s2.value)
        }
      } else {
        s1.nbJ.compareTo(s2.nbJ)
      }
    } else {
      -  s1.game.compareTo(s2.game)
    }
  }


}

object Test extends App {

  implicit val template = withBootestrap
  val s = ScoreTest.randomSeq(100)
  val scores = Scores(s)


  println(htmlTemplateSorted(scores))
}