package bon.jo.main

import bon.jo.conf.{Conf, SerUNerOption, SerUnserUtil}
import bon.jo.model.Scores

import scala.io.StdIn

object ConsoleScores extends App {
  implicit val opt: SerUNerOption = Conf.outFile.copy(filePath = Conf.outFile.filePath + "_server")
  val exits = Set(null, "", "exit", "quit")
  implicit val p : String = Conf.outFile+"_server"
  val scores: Scores = SerUnserUtil.readObject(null)
  scores.reduce
  scores.scores.zipWithIndex.sorted.map(e => s"${e._2} => ${e._1}").foreach(println)

  var userLine = StdIn.readLine("chose index to remove")
  while (!(exits contains userLine)) {
    val i = userLine.toInt
    scores.scores = scores.scores.filter(_ != scores.scores(i))
    scores.scores.zipWithIndex.map(e => s"${e._2} => ${e._1}").foreach(println)
    userLine = StdIn.readLine("chose index to remove")
  }
  SerUnserUtil.writeObject(scores)
}
