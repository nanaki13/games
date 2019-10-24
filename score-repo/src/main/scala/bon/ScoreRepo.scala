package bon


import java.io.PrintWriter
import java.sql.{Connection, DriverManager}
import java.util.logging.Logger

import bon.jo.DoIt
import bon.jo.FutureUtil._
import com.typesafe.config.ConfigFactory
import javax.sql.DataSource

import scala.concurrent.Future
import scala.jdk.CollectionConverters._



trait ScoreRepo extends ScoreRepoTables with WithProfile {



  implicit def ec: scala.concurrent.ExecutionContext

  val db = initDb()

  def all: Future[Seq[(User, Score)]] = {
    val res = joinUserScoreResult()
    db.run(res)
  }


  import profile.api._

  def pr[T](n: T): T = {
    println(n);
    n
  }

  println("loading : " + profileName)

  def mStrin(k: String, v: AnyRef): (String, String) = (k, v.toString)


  def initDb() =  if (urlDb.isEmpty) {

    Database.forConfig(profileName)
  } else {
    val conf = ConfigFactory.load
    println("initial db config = "+conf.getConfig(profileName))
    val confMap = conf.getObject(profileName).unwrapped().asScala.map((e: (String, AnyRef)) => {
      mStrin(e._1, e._2)
    })
    confMap += ("url" -> urlDb.get)
    confMap += ("user" -> userDb.get)
    confMap += ("password" -> passwordDb.get)
    val compConf = ConfigFactory.parseMap(Map(profileName -> confMap.asJava).asJava)
    Database.forConfig(profileName, compConf)
  }


  def createTables: Future[Int] = {
    db.run(DBIO.sequence(tables.map(_.schema.createIfNotExists)).map(l => l.count(_ => true)))
  }

  def drop(table: String) = {
    val sql = sqlu"DROP TABLE #${table.toUpperCase} CASCADE"
    println(sql.statements)
    sql
  }

  def dropTaable: Future[Int] = {
    val big = tables.reverse.map { table => drop(table.baseTableRow.tableName) }
    val forComp = for {
      f <- big.map(db.run(_))

    } yield f.map(_ => 1)
    Future.sequence(forComp).map(_.sum)
  }

  private object newId {

    private var id = (DoIt now db.run(users.map(_.id).max.result)).getOrElse(0)

    def get(): Int = this.synchronized {
      id += 1
      id
    }
  }

  val findByUserName = users.findBy(_.name).applied(_)

  def throwEx[R](testElement: R => Boolean)(element: R): R = {
    if (testElement(element)) {
      throw new Exception("bad expected response")
    }
    element
  }

  def createScore(sc: Score): Future[Score] = {
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

      val id = newId.get()
      println(id)
      val newOne = User(uName, id)
      db.run((for {
        insertResult <- users += newOne
      } yield {
        println(id, newOne)
        if (insertResult == 1) {
          newOne
        } else {
          throw new Exception("cant create user")
        }
      }).transactionally)
    }
  }

  def createUserIfNotExists(uName: String): Future[User] = {
    println("begin createUserIfNotExists")
    val q = findByUserName(uName)
    for {
      uO <- db.run(q.result.headOption)
      e: User <- uO match {
        case Some(u: User) => println(s"retived : ${u}"); Future.successful(u)
        case None => println(s"new One for ${uName}"); createUser(uName)
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
  def addScore(u: User, s: Score): Future[(User, Score, Int)] = {
    val opp = for {
      op <- {
        db.run(usersScores += UserScore(u.id, s.id))
      }
    } yield (u, s, op)
    opp
  }

  private def joinUserScoreFor(uu: User, game: String): Query[ScoresTable, Score, Seq] = for {
    u <- users
    us <- usersScores if u.id === us.idUser && u.id === uu.id
    s <- scores if s.id === us.idScore && s.game === game
  } yield s

  private def joinUserScoreResult(uu: User, game: String) = joinUserScoreFor(uu, game).result

  private def joinUserScoreFor = for {
    u <- users
    us <- usersScores if u.id === us.idUser
    s <- scores if s.id === us.idScore
  } yield (u, s)

  private def joinUserScoreResult() = joinUserScoreFor.result

  def getScore(uu: User, game: String): Future[(User, Seq[Score])] = {
    //   val j = users join usersScores on( _.id === _.idUser) join scores on( _._2.idScore === _.id)
    val quesry = joinUserScoreResult(uu, game)
    db.run(quesry).map {
      (uu, _)
    }
  }


}





