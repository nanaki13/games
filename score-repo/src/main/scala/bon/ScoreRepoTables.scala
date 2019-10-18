package bon



trait ScoreRepoTables {
  this: WithProfile =>

  import profile.api._



  class ScoresTable(tag: Tag) extends Table[Scores](tag, "SCORE") {
    // This is the primary key column:
    def id = column[Int]("id", O.PrimaryKey)

    def score = column[Int]("score")

    def game = column[String]("game")

    def when = column[Long]("date")

    def * =
      (id, score, game, when) <> (Scores.tupled, Scores.unapply)
  }



  class UserTable(tag: Tag) extends Table[User](tag, "USER") {

    def id = column[Int]("id", O.PrimaryKey)

    def name = column[String]("name")

    def * =
      (name,id) <> (User.tupled, User.unapply)
  }

  class UserScoreTable(tag: Tag) extends Table[UserScore](tag, "USER_SCORE") {
    // This is the primary key column:
    def idUser = column[Int]("id_user", O.PrimaryKey)

    def idScore = column[Int]("id_score")

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
