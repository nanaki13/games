package bon.jo

import bon.{ScoreRepo, Scores, User}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MitronScoreService {
  type ScoresType = bon.jo.model.Score

  def repo: ScoreRepo

  def save(s: ScoresType): Future[(Scores, Seq[User])] = {
    val users = s.who
    val score = Scores(0, s.value, s.game, s.when.toEpochDay * 24 * 3600)

    def ff(s: Scores): Seq[Future[(User, Scores, Int)]] = for {
      u <- users
    } yield {
      repo.addScore(User(u), s)
    }

    val futures: Future[Seq[(User, Scores, Int)]] =


      for {
        savedScore <- repo.createScore(score)
        resFromRepo <- Future.sequence(ff(savedScore))

      } yield {
        resFromRepo
      }
    futures.map(groupByScoreFirst)
  }

  def groupByScoreFirst(p: Seq[(User, Scores, Int)]): (Scores, Seq[User]) = {
    println(p)
    val tmpRes = p.groupBy(_._2).head
    (tmpRes._1, tmpRes._2.map(_._1))
  }
}
