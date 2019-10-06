package model

import model.Model.{Direction, Pos}

object Speed {
  def apply(x: Int, y: Int): Speed = {
    new Speed(x, y)
  }

  val None = Speed(0, 0)
  implicit val builder = (x: Int, y: Int) => Speed(x, y)
}

case class Speed(x: Int, y: Int) extends Pos {
  def toAproDirection: Direction = {

    (x, y) match {
      case (a, b) if (a > 0) => Direction.right
      case (a, b) if (a < 0) => Direction.left
      case (a, b) if (b > 0) => Direction.down
      case (a, b) if (b < 0) => Direction.up
      case _ => Direction.none
    }
  }


  override def trX: Speed = copy(-x, y)

  override def trY: Speed = copy(x, -y)
}