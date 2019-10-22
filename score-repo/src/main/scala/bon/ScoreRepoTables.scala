package bon

import java.time.LocalDate


trait ScoreRepoTables {
  this: WithProfile =>

  import profile.api._



  class ScoresTable(tag: Tag) extends Table[Score](tag, "score") {
    // This is the primary key column:
    def id = column[Int]("id", O.PrimaryKey)

    def score = column[Int]("score")

    def nbPlayer = column[Int]("nb_player")

    def game = column[String]("game")

    def when = column[LocalDate]("date")

    def * =
      (id, score,nbPlayer, game, when) <> (Score.tupled, Score.unapply)
  }



  class UserTable(tag: Tag) extends Table[User](tag, "user") {

    def id = column[Int]("id", O.PrimaryKey)

    def name = column[String]("name")

    def * =
      (name,id) <> (User.tupled, User.unapply)
  }

  class UserScoreTable(tag: Tag) extends Table[UserScore](tag, "user_score") {
    // This is the primary key column:
    def idUser = column[Int]("id_user")

    def idScore = column[Int]("id_score")

    def pk = primaryKey("pk_user_score", (idUser, idScore))
    def user = foreignKey("user_fk", idUser, users)(_.id/*, onUpdate = ForeignKeyAction.Restrict, onDelete = ForeignKeyAction.Cascade*/)

    def score = foreignKey("score_fk", idScore, scores)(_.id/*, onUpdate = ForeignKeyAction.Restrict, onDelete = ForeignKeyAction.Cascade*/)

    def * =
      (idUser, idScore) <> (UserScore.tupled, UserScore.unapply)
  }

  lazy val scores = TableQuery[ScoresTable]
  lazy val users = TableQuery[UserTable]
  lazy val usersScores = TableQuery[UserScoreTable]

  def tables = List(scores, users, usersScores)


}
