package bon.jo.model

import java.time.LocalDate

import scala.util.Random



case class Score(
                game : String,
                  when: LocalDate,
                  value: Int,
                  who:List[String]
                ) extends Ordered[Score]{
  override def compare(that: Score): Int = this.value - that.value
  def nbJ: Int = who.size
  def  + (v:Int) : Score = this.copy(value = this.value+v)
  def tuUiString : String =  s"$value : ${who.mkString(" & ")}"
}



object Score{
  val None : Score = Score("",LocalDate.EPOCH,0,Nil)

  object ScoreTest {

    val chars: Seq[Char] = ('a' to 'z') ++ ('A' to 'Z') ++ ('1' to '9')

    def nextString(value: 10) = {
      (for{
        a <- 0 to 10
        c = chars(Random.nextInt(chars.length))
      }yield c).mkString
    }

    def random = Score("Mitron",LocalDate.of(randomYear, randomMonth, randomDay), Random.nextInt(10000),List(nextString(10)))

    def randomMonth = Random.nextInt(12) + 1

    def randomDay = Random.nextInt(27) + 1

    def randomYear = Random.nextInt(3) + 2017

    def randomSeq(nb: Int) = for {_ <- 1 to nb} yield {
      random
    }


  }
}
