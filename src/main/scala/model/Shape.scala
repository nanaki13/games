package model

import java.awt.Color

import model.Model.{BasePos, Colored}
import model.Shapes.{DirAndIdParam, ShapeParam, ShapeParamMultiple, ShapeParamOne, ShapeParamTwo}

object Shape {
  val Circle = 0
  val Point = 1
  val None = 2
  val Rectangle = 3
  val ComposedShape = 4
  val Image = 5

  def apply(shape: Int): Int => Shape = r => new Shape(shape, ShapeParamOne(r,Color.black))

  def apply(shape: Int, color: String): Int => Shape = r => new Shape(shape, ShapeParamOne(r, Color.decode(color)))

}

case class Shape(shape: Int, param: ShapeParam ) extends Colored{
  override def awtColor: Color = param.color
  def getMaxNormFromCenter:Double = {
    this.shape match {
      case Shape.Circle => this.param.specific[ShapeParamOne[Int]].x
      case Shape.Point => this.param.specific[ShapeParamOne[Int]].x/2d
      case Shape.Rectangle => BasePos(this.param.specific[ShapeParamTwo].x/2,this.param.specific[ShapeParamTwo].y/2 ).nrm
      case Shape.ComposedShape => this.param.specific[ShapeParamMultiple].maxLenghtFromCenter
      case Shape.Image => BasePos(this.param.specific[DirAndIdParam].w/2,this.param.specific[DirAndIdParam].h/2).nrm

    }
  }
}