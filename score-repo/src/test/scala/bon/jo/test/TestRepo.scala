package bon.jo.test

import bon.jo.FutureUtil._
import bon.{Scores, User}
import org.scalatest.{FlatSpec, _}

class TestRepo extends FlatSpec with Matchers with TestRepoImpl {

  ~ createTables
  val s = Scores(0, 120, "mitron")
  var sWithId : Scores = _
  val user: User = ~ createUserIfNotExists("billTestRepo")


  "create User If Not Exists" should "retieve the same user" in {
    val user: User = ~ createUserIfNotExists("billTestRepo")
    val user2: User = ~ createUserIfNotExists("billTestRepo")

    user should be(user2)
  }

  "add User and score" should "retieve the same user and score" in {


    sWithId = ~ createScore(s)

    val (fromRepoUser,fromRepoScore,cnt) = ~ addScore(user,sWithId)

    sWithId = s.copy(id= fromRepoScore.id)
    fromRepoUser should be(user)
    fromRepoScore should be(sWithId)
    cnt should be(1)
  }

  "get user and score" should "be the same as before" in {
    val(fromRepoUser,fromRepoScore) =  ~ getScore(user,"mitron")
    fromRepoUser should be(user)
    fromRepoScore(0) should be(sWithId)

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


}

//object DB extends H2 with ScoreRepo with WithProfile {
//
//}
