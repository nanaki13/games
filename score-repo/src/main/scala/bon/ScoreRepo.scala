package bon

import java.time.Instant
import java.util.Date

import bon.ScoreRepo.sc
import com.typesafe.config.{Config, ConfigFactory}
import slick.driver.JdbcProfile
import slick.jdbc.JdbcProfile
import slick.jdbc.meta.MTable

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

trait WithProfile {
  val profile: JdbcProfile

  def profileName: String
}

trait ScoreRepo {
  this: WithProfile =>

  import profile.api._


  // type Scores = (Int, String,String,Float,Float,Int )
  case class Scores(id: Int =0, score: Int, game: String, when: Long = System.currentTimeMillis())

  class ScoresTable(tag: Tag) extends Table[Scores](tag, "SCORE") {
    // This is the primary key column:
    def id = column[Int]("id", O.PrimaryKey)

    def score = column[Int]("score")

    def game = column[String]("game")

    def when = column[Long]("date")


    // Every table needs a * projection with the same type as the table's type parameter
    def * =
      (id, score, game, when) <> (Scores.tupled, Scores.unapply)
  }

  // type Scores = (Int, String,String,Float,Float,Int )
  case class User(id: Int = 0, name: String)

  class UserTable(tag: Tag) extends Table[User](tag, "USER") {
    // This is the primary key column:
    def id = column[Int]("id", O.PrimaryKey)

    def name = column[String]("game")

    // Every table needs a * projection with the same type as the table's type parameter
    def * =
      (id, name) <> (User.tupled, User.unapply)
  }

  // type Scores = (Int, String,String,Float,Float,Int )
  case class UserScore(idUser: Int, idScore: Int)

  class UserScoreTable(tag: Tag) extends Table[UserScore](tag, "USER_SCORE") {
    // This is the primary key column:
    def idUser = column[Int]("id_user", O.PrimaryKey)

    def idScore = column[Int]("id_score")

    def user = foreignKey("user_fk", idUser, users)(_.id/*, onUpdate = ForeignKeyAction.Restrict, onDelete = ForeignKeyAction.Cascade*/)

    def score = foreignKey("score_fk", idScore, scores)(_.id/*, onUpdate = ForeignKeyAction.Restrict, onDelete = ForeignKeyAction.Cascade*/)

    // Every table needs a * projection with the same type as the table's type parameter
    def * =
      (idUser, idScore) <> (UserScore.tupled, UserScore.unapply)
  }

  lazy val scores = TableQuery[ScoresTable]
  lazy val users = TableQuery[UserTable]
  lazy val usersScores = TableQuery[UserScoreTable]

  def tables = List(scores, users, usersScores)


}

case class H2(profile: JdbcProfile = slick.jdbc.H2Profile, profileName: String = "h2")

object ScoreRepo extends App {

  def pr[T](n: T): T = {
    println(n);
    n
  }

  object sc extends H2 with ScoreRepo with WithProfile

  import sc.profile.api._
  import sc._
  import scala.concurrent.ExecutionContext.Implicits.global

  val db = Database.forConfig("h2mem1")

  def createTables = {
    db.run(MTable.getTables).map(v => {
      val names = v.map(mt => mt.name.name)
      val createIfNotExist = tables.filter(table =>
        !names.contains(table.baseTableRow.tableName)).map(_.schema.create)
      db.run(DBIO.sequence(createIfNotExist))
    }).flatten
    //    val existing = db.run(MTable.getTables)
    //    val f = existing.flatMap(v => {
    //      val names = v.map(mt => mt.name.name)
    //      val createIfNotExist = tables.filter(table =>
    //        !names.contains(table.baseTableRow.tableName)).map(_.schema.create)
    //      db.run(DBIO.sequence(createIfNotExist))
    //
    //    })
    //
    //    f
  }

  val action = sc.scores += Scores(0, 3, "miton", System.currentTimeMillis())
  val findByUserName = users.findBy(_.name).applied(_)
//  val findByUserName = users.findBy(_.name).applied(_)



  // val createAndAdd = Future.sequence(List(createTables, actionAndPrint))
  def throwEx[R](testElement: R => Boolean)(element: R): R = {
    if (testElement(element)) {
      throw new Exception("bad expected response")
    }
    element
  }

  def createScore(sc : Scores): Future[Scores] = {
    for{
      maxIdOption <- db.run(scores.map(_.id).max.result)
      maxId  = maxIdOption.getOrElse(0)
      newOne = sc.copy(id = maxId+1)
      insertResult <- db.run(scores +=newOne)
    }yield {
      if(insertResult == 1){
        newOne
      }else{
        throw new Exception("cant create score")
      }

    }
  }

  def createUser(uName: String): Future[User] = {
    for{
      maxIdOption <- db.run(users.map(_.id).max.result)
      maxId  = maxIdOption.getOrElse(0)
      newOne = User(maxId+1,uName)
      insertResult <- db.run(users +=newOne)
    }yield {
      if(insertResult == 1){
        newOne
      }else{
        throw new Exception("cant create score")
      }
    }
  }

  def createUserIfNotExists(uName: String): Future[User] = {
    val q = findByUserName(uName)
    for {
      uO <- db.run(q.result.headOption)
      e: User <- uO match {
        case Some(u: User) => println(s"retived : ${u}"); Future.successful(u)
        case None =>  println("new One");createUser(uName)
        case _ => throw new Exception("bad expected response")
      }
    } yield e
  }


  val scorerres = Await.result(createTables, Duration.Inf)

  println(scorerres)

  val res = Await.result(createUserIfNotExists("bob"), Duration.Inf)

  println(res)


  val uu = User(name = "bob")
  val ss = Scores(score = 1000,game = "mitrson")

  def addScore(u  : User, s : Scores) ={
    val opp = for{
      user <- createUserIfNotExists(u.name)
      score <- createScore(s)
      op <- db.run(usersScores += UserScore(user.id,score.id))
    }yield  (user,score,op)
    opp
  }

  def getScore(uu : User,game : String): Future[(sc.User, Seq[sc.Scores])] = {
     //   val j = users join usersScores on( _.id === _.idUser) join scores on( _._2.idScore === _.id)
        val joinWall =  for{
          u <- users
          us <- usersScores if u.id === us.idUser && u.id===uu.id
          s <- scores if s.id === us.idScore && s.game === game
        } yield s


    println(joinWall.result.statements)
    db.run(joinWall.result).map{
      (uu,_)
    }
  }


  val resAdd =  Await.result(addScore(uu,ss), Duration.Inf)

Await.result(getScore(resAdd._1,"mitron").map(println(_)), Duration.Inf)

  Await.result( db.run( scores.result.map(println(_))), Duration.Inf)
  Await.result( db.run( users.result.map(println(_))), Duration.Inf)
  Await.result( db.run( usersScores.result.map(println(_))), Duration.Inf)

}



