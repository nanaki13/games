package bon.jo



import bon.jo.controller.Scores
import bon.{Score, ScoreRepo, ScoreUtil, User}

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

trait ScoreServiceImpl extends ScoreService {


  def repo: ScoreRepo

  def save(s: ScoresType): Future[(Score, Seq[User])] = {
    println("begin save")
    val users = s.who
    val score = Score(0, s.value, s.who.size, s.game, s.when)

    val createUansdS = for {
      newUsers <- Future.sequence(users.map(repo.createUserIfNotExists(_)))
      savedScore <- repo.createScore(score)
      ret <- Future.sequence(newUsers.map(repo.addScore(_, savedScore)))
    } yield ret

    createUansdS.map(groupByScoreFirst)
  }

  def all: Future[Iterator[ScoresType]] = repo.all.map {
    e => {
      e.groupMap(_._2)(_._1).iterator.map(e => ScoreUtil.toCommonScore(e._1, e._2))
    }
  }


  private def groupByScoreFirst(p: Seq[(User, Score, Int)]): (Score, Seq[User]) = {
    val tmpRes = p.groupBy(_._2).head
    (tmpRes._1, tmpRes._2.map(_._1))
  }
}

trait ScoreService {
  type ScoresType = bon.jo.model.Score


  def save(s: ScoresType): Future[(Score, Seq[User])]

  def all: Future[Iterator[ScoresType]]

  def allInScores: Future[Scores] = all.map(e => Scores(e.toList))

}

