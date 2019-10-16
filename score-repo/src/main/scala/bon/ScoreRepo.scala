package bon

import java.time.Instant
import java.util.Date

import slick.driver.JdbcProfile
import slick.jdbc.JdbcProfile

trait WithProfile {
  val profile: JdbcProfile
  def profileName : String
}

trait ScoreRepo {
  this: WithProfile =>

  import profile.api._


  // type Oeuvres = (Int, String,String,Float,Float,Int )
  case class Oeuvres(val id: Int, val name: String, val score: Int, val game: String, val when: Instant)

  class OeuvresTable(tag: Tag) extends Table[Oeuvres](tag, "Score") {
    // This is the primary key column:
    def id= column[Int]("id", O.PrimaryKey, O.AutoInc)

    def name = column[String]("name")

    def score = column[Int]("score")

    def game = column[String]("game")

    def when= column[Instant]("date")


    // Every table needs a * projection with the same type as the table's type parameter
    def * =
      ( id,  name,  score,  game,  when) <> (Oeuvres.tupled, Oeuvres.unapply)
  }

  lazy val ouvres = TableQuery[OeuvresTable]
  ouvres +=  Oeuvres(0, "test",3, "miton", Instant.now())
}

case class H2(profile: JdbcProfile =slick.jdbc.H2Profile,profileName : String = "h2")

object Test{
  object sc extends H2 with ScoreRepo with WithProfile
  import sc.profile.api._
  import sc._
  val action =  sc.ouvres +=  Oeuvres(0, "test",3, "miton", Instant.now())

}