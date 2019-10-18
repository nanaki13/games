package bon.jo.test

import java.time.LocalDate
import bon.jo.FutureUtil._
import bon.ScoreRepo
import bon.jo.MitronScoreService
import org.scalatest._


class TestService extends FlatSpec with Matchers {

  object repoTest extends TestRepoImpl
  ~ repoTest.createTables
  sealed trait serviceTrait extends MitronScoreService {
    override def repo: ScoreRepo = repoTest
  }

  object service extends serviceTrait

  "saved base score" should "same as scoreInput" in {
   val scoreInput =  bon.jo.model.Score(
    "mitron",
      LocalDate.now,
      123,
      List("bob","roger")
    )
    val res = ~ service.save(scoreInput)

    res._2.map(_.name).toSet should be(Set("bob","roger"))
    res._1.game should be ("mitron")
    res._1.score should be (123)
    res._1.when should be (LocalDate.now.toEpochDay * 3600 * 24)

  }

}
