package bon.jo.test

import bon.jo.DoIt
import bon.jo.FutureUtil._
import bon.{Score, User}
import org.scalatest.{FlatSpec, _}

import scala.concurrent.ExecutionContext

class TestRepo extends FlatSpec with Matchers with TestRepoImpl {

 // ~ dropTaable
// ~ createTables
  val s = Score(0,120, 1, "mitron")
  var sWithId : Score = _

  val user: User = DoIt now createUserIfNotExists("billTestRepo")


  "create User If Not Exists" should "retieve the same user" in {
    val user: User = DoIt now createUserIfNotExists("billTestRepo")
    val user2: User = DoIt now createUserIfNotExists("billTestRepo")

    user should be(user2)
  }

  "add User and score" should "retieve the same user and score" in {


    sWithId = DoIt now createScore(s)

    println("sWithId",sWithId)
    println("user",user)
    val (fromRepoUser,fromRepoScore,cnt) = DoIt now addScore(user,sWithId)

    sWithId = s.copy(id= fromRepoScore.id)
    fromRepoUser should be(user)
    fromRepoScore should be(sWithId)
    cnt should be(1)
  }

  "get user and score" should "be the same as before" in {
    val(fromRepoUser,fromRepoScore) =   DoIt now getScore(user,"mitron")
    fromRepoUser should be(user)
    fromRepoScore.last should be(sWithId)

  }


  //  createTables resolve

  //
  //  println(user,user2)
  //  user should be (user2)


  //  Await.result(getScore(resAdd._1,"mitron").map(println(_)), Duration.Inf)
  //
  //  Await.result( db.run( scores.result.map(println(_))), Duration.Inf)
  //  Await.result( db.run( users.result.map(println(_))), Duration.Inf)
  //  Await.result( db.run( usersScores.result.map(println(_))), Duration.Inf)
  override implicit def ec: ExecutionContext = scala.concurrent.ExecutionContext.global
}

//object DB extends H2 with ScoreRepo with WithProfile {
//
//}
