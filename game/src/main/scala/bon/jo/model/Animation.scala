package bon.jo.model

object Animation {
  case class DieWithCount(cnt : Int ,override val shapeTr :Option[Shape => Shape]= None) extends AnimationProp{
    var _countBeforeDi = cnt

    override def canDie: Boolean = true

    override def countBeforeDi: Int = {

      _countBeforeDi -= 1
      _countBeforeDi
    }
  }
  trait AnimationProp {
    def canDie : Boolean
    def countBeforeDi : Int
    def shapeTr :Option[Shape => Shape]=None
  }

}
