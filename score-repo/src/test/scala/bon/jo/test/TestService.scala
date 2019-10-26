package bon.jo.test

import java.time.LocalDate

import bon.jo.FutureUtil._
import bon.ScoreRepo
import bon.jo.conf.{SerUNerOption, SerUnserUtil}
import bon.jo.controller.Scores
import bon.jo.model.Score
import bon.jo.{DoIt, ScoreServiceImpl}
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import  bon.jo.ImpliciErroMessage._
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

    val ress = DoIt now service.save(scoreInput)
    val res = ress.get
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

object AddScore extends App{
  object repoTest extends TestRepoImpl {
    override implicit def ec:   ExecutionContext  =   scala.concurrent.ExecutionContext.Implicits.global
  }
  case class service( repo: ScoreRepo) extends ScoreServiceImpl

  val ser = new service(repoTest)

// val translate =  SerUnserUtil.readObject(Scores.empty)(SerUNerOption("I:\\work\\process\\mitronServeur\\data.v1.0_server"))
//  translate.scores.foreach(s => (DoIt now ser.save(s)))
  DoIt now ser.save(Score("mitron_v0.1",
    LocalDate.now(),6710,List("MasterQpuc")))
  DoIt now ser.save(Score("mitron_v0.1",
    LocalDate.now(),7710,List("Filex")))
  DoIt now ser.save(Score("mitron_v0.1",
    LocalDate.now(),4950,List("Jonath")))
  DoIt now ser.save(Score("mitron_v0.1",
    LocalDate.now(),1300,List("Ninoo")))
  DoIt now ser.save(Score("mitron_v0.1",
    LocalDate.now(),1200,List("Fliles")))
}