package bon

import java.time.{Instant, LocalDate, ZoneId}

case class Score(id: Int = 0, score: Int, nbPlayers: Int, game: String, when: LocalDate = LocalDate.now())

object ScoreUtil {
  type CommonScore = bon.jo.model.Score

  def toCommonScore(score: Score, list: Seq[User]): CommonScore = {
    bon.jo.model.Score(score.game,
     score.when,
      score.score,
      list.map(_.name).toList)
  }
}
