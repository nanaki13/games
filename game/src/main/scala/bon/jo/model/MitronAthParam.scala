package bon.jo.model

case class MitronAthParam(score : Score,maxScore : Score,nbBullet : Int)extends AthParam
object MitronAthParam{
  val None = MitronAthParam(Score.None,  Score.None,0)
}
