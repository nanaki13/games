package bon.jo.controller

import bon.jo.controller.Score.ScoreTest
import Scores._

case class Scores(var scores: Seq[Score]) {
  def bestScoreListe(implicit nbPlayer: Int,game: String): Seq[Score] = scores.filter(nbPlayerAndGame).sorted.reverse

  def addIfBest(sc: Score)(implicit nbPlayer: Int, game: String):Boolean = {
    val min_ = min
    if(size < 20 || sc > min_) {
      add(sc)
      if(size > 20){
        scores = scores.filter(_ != min_)
      }
      true
    } else{
      false
    }
  }


  def add(scoreMax: Score) = {this.scores = scoreMax +: scores  }
  def +(s : Score) = add(s)
  implicit val order:Ordering[Score]= (s1: Score, s2: Score) => s1.value - s2.value

  def maxByMonth: Map[(Int, Int), Score] = scores.map {
    e => {
      (e.when.getYear, e.when.getMonthValue, e)
    }
  } groupBy {
    e => (e._1, e._2)
  } map {
    e => {
      (e._1, e._2.map(_._3).max)
    }
  }
  def max(implicit nbPlayer: Int,game: String): Score = {
    val fl =  scores.filter(nbPlayerAndGame)
    if( fl.isEmpty){
      Score.None
    } else{
      fl.max
    }
  }
  def min(implicit nbPlayer: Int ,game: String): Score ={
    val fl =  scores.filter(nbPlayerAndGame)
    if( fl.isEmpty){
      Score.None
    } else{
      fl.min
    }
  }
  def size(implicit nbPlayer: Int, game: String): Int = scores.count(_.who.size == nbPlayer)
}

object Scores {
  def nbPlayerAndGame(score: Score)(implicit nbPlayer: Int, game: String) : Boolean = {score.game == game && score.nbJ == nbPlayer}
  val empty: Scores = Scores(Nil)
  def randrom(nb : Int) = Scores(ScoreTest.randomSeq(nb))
}
