package bon.jo.test

import java.time.LocalDate

import bon.jo.FutureUtil._
import bon.ScoreRepo
import bon.jo.{DoIt, ScoreServiceImpl}
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

class TestService extends FlatSpec with Matchers {

  object repoTest extends TestRepoImpl {
    override implicit def ec: ExecutionContext =   scala.concurrent.ExecutionContext.Implicits.global
  }
 // ~ repoTest.dropTaable
  //~ repoTest.createTables
  sealed trait serviceTrait extends ScoreServiceImpl {
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
    val res = DoIt now service.save(scoreInput)

    res._2.map(_.name).toSet should be(Set("bob","roger"))
    res._1.game should be ("mitron")
    res._1.score should be (123)
    res._1.when should be (LocalDate.now)

  }

}
object dropRepo extends App{
  object repoTest extends TestRepoImpl {
    override implicit def ec: ExecutionContext  =   scala.concurrent.ExecutionContext.Implicits.global
  }
  DoIt now repoTest.dropTaable
}

object createRepo extends App{
  object repoTest extends TestRepoImpl {
    override implicit def ec:   ExecutionContext  =   scala.concurrent.ExecutionContext.Implicits.global
  }
  DoIt now repoTest.createTables
}