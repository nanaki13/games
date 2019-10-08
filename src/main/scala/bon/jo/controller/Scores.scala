package bon.jo.controller

case class Scores(var scores: Seq[Score]) {
  def add(scoreMax: Score) = {this.scores = scoreMax +: scores  }

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
  def max: Score = scores.max
}

object Scores {
  val empty: Scores = Scores(Nil)
}
