package bon

import slick.jdbc.meta.MTable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import bon.jo.FutureUtil._

trait ScoreRepo extends ScoreRepoTables with WithProfile {

  import profile.api._

  def pr[T](n: T): T = {
    println(n);
    n
  }

  val db = Database.forConfig(profileName)

  def createTables = {
    db.run(MTable.getTables).map(v => {
      val names = v.map(mt => mt.name.name)
      val createIfNotExist = tables.filter(table =>
        !names.contains(table.baseTableRow.tableName)).map(_.schema.create)
      db.run(DBIO.sequence(createIfNotExist))
    }).flatten

  }


  val findByUserName = users.findBy(_.name).applied(_)

  def throwEx[R](testElement: R => Boolean)(element: R): R = {
    if (testElement(element)) {
      throw new Exception("bad expected response")
    }
    element
  }

  def createScore(sc: Scores): Future[Scores] = {
    for {
      maxIdOption <- db.run(scores.map(_.id).max.result)
      maxId = maxIdOption.getOrElse(0)
      newOne = sc.copy(id = maxId + 1)
      insertResult <- db.run(scores += newOne)
    } yield {
      if (insertResult == 1) {
        newOne
      } else {
        throw new Exception("cant create score")
      }

    }
  }

  def createUser(uName: String): Future[User] = {
    synchronized {
      val maxIdOptionOp = ~db.run(users.map(_.id).max.result)
      val newOne = User(uName, maxIdOptionOp.getOrElse(0) + 1)
      db.run((for {
        insertResult <- users += newOne
      } yield {
        println(maxIdOptionOp, newOne)
        if (insertResult == 1) {
          newOne
        } else {
          throw new Exception("cant create score")
        }
      }).transactionally)
    }
  }

  def createUserIfNotExists(uName: String): Future[User] = {
    val q = findByUserName(uName)
    for {
      uO <- db.run(q.result.headOption)
      e: User <- uO match {
        case Some(u: User) => println(s"retived : ${u}"); Future.successful(u)
        case None => println("new One"); createUser(uName)
        case _ => throw new Exception("bad expected response")
      }
    } yield e
  }


  /**
    * Score must exists
    *
    * @param u
    * @param s
    * @return
    */
  def addScore(u: User, s: Scores): Future[(User, Scores, Int)] = {
    val opp = for {
      user <- createUserIfNotExists(u.name)
      op <- { println(~ db.run(usersScores.result));println(user,s,UserScore(user.id, s.id)); db.run(usersScores += UserScore(user.id, s.id))}
    } yield (user, s, op)
    opp
  }

  def getScore(uu: User, game: String): Future[(User, Seq[Scores])] = {
    //   val j = users join usersScores on( _.id === _.idUser) join scores on( _._2.idScore === _.id)
    val joinWall = for {
      u <- users
      us <- usersScores if u.id === us.idUser && u.id === uu.id
      s <- scores if s.id === us.idScore && s.game === game
    } yield s
    db.run(joinWall.result).map {
      (uu, _)
    }
  }


}



