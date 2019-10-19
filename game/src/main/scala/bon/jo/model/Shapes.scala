package bon.jo.model

import java.awt.Color

import bon.jo.model.Model.{BaseModel, Direction, ModelElement}

import scala.collection.mutable.ListBuffer

object Shapes {
  val defColor = Color.black

  def Circle(r: Int): Shape = Shape(Shape.Circle)(r)

  def Point(l: Int): Shape = Shape(Shape.Point)(l)

  def ComposedShape(in: ListBuffer[ModelElement], maxLenghtFromCenter: Double): Shape = ComposedShapes(Shape.ComposedShape,in,maxLenghtFromCenter)

  def Rectangle(w: Int, h: Int): Shape = XYShape(Shape.Rectangle)(w, h)

  def Image(id: String, initialDire: Direction,w : Int,h : Int) = Shape(Shape.Image, DirAndIdParam(id, initialDire,w,h))

  def Circle(col: Color)(r: Int): Shape = Shape(Shape.Circle, ShapeParamOne(r, col))

  def Point(col: Color)(l: Int): Shape = Shape(Shape.Point, ShapeParamOne(l, col))

  def Rectangle(col: Color): (Int, Int) => Shape = XYShape(Shape.Rectangle, col)

  val None: Shape = Shape(Shape.None)(0)

  trait ShapeParam {
    def color: Color

    def specific[T <: ShapeParam] = this.asInstanceOf[T]
  }

  trait DeaultColor extends ShapeParam {
    def color: Color = Color.BLACK
  }

  case class ShapeParamOne[T](x: T, color: Color) extends ShapeParam

  case class DirAndIdParam(x: String, direction: Direction,w: Int,h: Int) extends ShapeParam with DeaultColor

  case class ShapeParamTwo(x: Int, y: Int, color: Color) extends ShapeParam

  case class ShapeParamMultiple(inner: ListBuffer[ModelElement], maxLenghtFromCenter: Double) extends ShapeParam with DeaultColor


  object XYShape extends DeaultColor {
    def apply(shape: Int): (Int, Int) => Shape = (x, y) => new Shape(shape, ShapeParamTwo(x, y, color))

    // def apply(shape: Int, color: String): (Int, Int) => Shape = (x, y) => new Shape(shape, ShapeParamTwo(x, y), Color.decode(color))
    def apply(shape: Int, color: Color): (Int, Int) => Shape = (x, y) => new Shape(shape, ShapeParamTwo(x, y, color))
  }

  object ComposedShapes {
    def apply(shape: Int, l: ListBuffer[ModelElement], maxLenghtFromCenter: Double): Shape = new Shape(shape, ShapeParamMultiple(l, maxLenghtFromCenter))

    //   def apply(shape: Int, color: String): ListBuffer[BaseModel] => Shape = (l) => new Shape(shape, ShapeParamMultiple(l), Color.decode(color))
  }

}
