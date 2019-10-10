package bon.jo.model

import java.awt.Color

import bon.jo.conf.Conf
import bon.jo.model.Animation.{AnimationProp, DieWithCount}
import bon.jo.model.Model.{BaseModel, ModelElement}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

trait Model {
  def elements: ListBuffer[ModelElement]

  def player1: ModelElement

  def player2: ModelElement
}

object Model {

  class Buulet(pos: Pos, speed: Speed, option :Option[AnimationProp]= Some(DieWithCount(2*100))) extends BaseModel(pos = pos
    , shape = Shapes.Image("bullet", Direction.right, 50, 50),
    speed = speed,
    group = Group.Bullet,option =option){
    override def _copy(pos_ : Pos, speed_ : Speed): ModelElement = new Buulet(pos = pos_, speed = speed_,option=option)
  }

  class BuletItem(pos: Pos, option :Option[AnimationProp]= Some(DieWithCount(5*100))) extends BaseModel(pos = pos
    , shape = Shapes.Image("item", Direction.up, 20, 20),
    group = Group.Item,option =option){
    override def _copy(pos_ : Pos, speed_ : Speed): ModelElement = new BuletItem(pos = pos_,option=option)
  }
  class NovaItem(pos: Pos, option :Option[AnimationProp]= Some(DieWithCount(5*100))) extends BaseModel(pos = pos
    , shape = Shapes.Image("item", Direction.up, 20, 20),
    group = Group.Item, option =option){
    override def _copy(pos_ : Pos, speed_ : Speed): ModelElement = new NovaItem(pos = pos_,option=option)
  }

  object Group {
    val Enemy = 0
    val Bullet = 1
    val User = 2
    val Source = 3
    val Item = 4
    val Background = 4
  }

  case class Direction(val direction: Short) {
    def toAngle: Int = this match {
      case Direction.up => -90
      case Direction.right => 0
      case Direction.down => 90
      case Direction.left => 180
      case _ => 0
    }


    override def toString: String = this match {
      case Direction.up => "up"
      case Direction.right => "right"
      case Direction.down => "down"
      case Direction.left => "left"
      case _ => "up"
    }

    def rotate90: Direction = {
      this match {
        case Direction.up => Direction.right
        case Direction.right => Direction.down
        case Direction.down => Direction.left
        case Direction.left => Direction.up
        case _ =>  Direction.none
      }
    }

  }

  object Direction {
    private val _up = 0.toShort
    private val _down = 1.toShort
    private val _left = 2.toShort
    private val _right = 3.toShort
    private val _none = 4.toShort
    val up = Direction(_up)
    val down = Direction(_down)
    val left = Direction(_left)
    val right = Direction(_right)
    val none = Direction(_none)
  }

  trait Colored {

    def awtColor: Color
  }

  trait BlackByDefault extends Colored {
    def color: String = "#000000"
  }

  trait ModelElement extends Pos with Colored with AnimationProp {
    def _copy(pos: Pos, speed: Speed = Speed.None): ModelElement

    def group: Int

    var speed: Speed

    var pos: Pos

    def x: Int = pos.x

    def y: Int = pos.y

    def shape: Shape

    override def awtColor: Color = shape.awtColor

  }

  object Pos {
    val org: Pos = BasePos(0, 0)

    def random(w: Int, h: Int): Pos = BasePos(Random.nextInt(w), Random.nextInt(h))

    def nrm(p1: Pos, p2: Pos): Double = {

      scala.math.sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y))


    }

  }

  trait Pos {
    def near(pos: Pos, value: Double) = Pos.nrm(this, pos) < value

    def nrm = {
      scala.math.sqrt(x * x + y * y)
    }

    def trY: Pos

    def trX: Pos

    def x: Int

    def y: Int

    def +(pos: Pos): Pos = BasePos(this.x + pos.x, this.y + pos.y)

    def *[T <: Pos](value: Float)(implicit builder: (Int, Int) => T): T = builder(x, y)
  }

  object Org extends BasePos(0, 0)

  case class BasePos(x: Int, y: Int) extends Pos {


    def trX: Pos = copy(-x, y)

    def trY: Pos = copy(x, -y)
  }

  def growToAndDown(implicit max: Double, grow: Boolean) = { (s: Shape) =>
    if (s.getMaxNormFromCenter > max && !grow) {
      Shapes.Circle(s.getMaxNormFromCenter.toInt - 10)
    } else {
      Shapes.Circle(s.getMaxNormFromCenter.toInt + 1)
    }
  }

  class CircleGrow(var _pos: Pos = Pos.org, maxGrow: Double,   grow :Boolean = true, shapeIni: Shape = Shapes.Circle(10), var _speed: Speed = Speed.None) extends AnimationModel(_pos, shapeIni,

    Group.Bullet, _speed
  ) {
    override def _copy(pos: Pos, speed: Speed): ModelElement = new CircleGrow(pos, maxGrow,_grow, nextShape, speed)
    var _grow = grow

    def speed_=(speed: Speed) = {
      this._speed = speed
    }

    def pos_=(pos: Pos) = {
      this._pos = pos
    }

    var maxFromCenter = shape.getMaxNormFromCenter



    override def canDie: Boolean = true

    override def countBeforeDi = {
      super.countBeforeDi
      maxFromCenter = shape.getMaxNormFromCenter

      if (maxFromCenter < 0) 0 else 1
    }


    override def tr(s: Shape): Shape = {

      if (s.getMaxNormFromCenter > maxGrow && _grow) {
        _grow = false
      }
      if (_grow) {
        Shapes.Circle(s.getMaxNormFromCenter.toFloat.round + 1)
      } else {
        Shapes.Circle(s.getMaxNormFromCenter.toFloat.round - 10)
      }
    }
  }

  abstract case class AnimationModel(pos: Pos,
                                     shapeIni: Shape,

                                     group: Int,
                                     speed: Speed = Speed.None,
                                     option: Option[AnimationProp] = None) extends ModelElement with AnimationProp {
    def tr(s: Shape): Shape

    var nextShape = shapeIni

    override def shapeTr: Option[Shape => Shape] = Some(tr)

    override def shape = nextShape

    override def canDie: Boolean = true

    override def countBeforeDi: Int = {
      nextShape = tr(shape)
      1
    }

    override def trY: Pos = pos.trX

    override def trX: Pos = pos.trY
  }

  case class BaseModel(var pos: Pos,
                       shape: Shape = Shapes.None,
                       group: Int,
                       var speed: Speed = Speed.None,
                       option: Option[AnimationProp] = None) extends ModelElement {


    def trY: BaseModel = copy(pos.trY)

    def trX: BaseModel = copy(pos.trX)


    override def canDie: Boolean = {
      option.isDefined && option.get.canDie
    }

    override def countBeforeDi: Int = if (option.isDefined) {
      option.get.countBeforeDi
    } else 1

    override def _copy(pos_ : Pos, speed_ : Speed): ModelElement = this.copy(pos = pos_, speed = speed_)
  }


  class Plateau(val w: Int, val h: Int) extends BaseModel(Org, Shapes.Rectangle(w, h), Group.Background, Speed.None)

  object PlateauBase extends Plateau(Conf.plateauSize._1, Conf.plateauSize._2)


  trait Refreh {
    def refresh
  }


  object PointModel {
    def apply(x: Int, y: Int, group: Int, speed: Speed = Speed.None): BaseModel = {
      new BaseModel(BasePos(x, y), Shapes.Point(5), group, speed)
    }
  }

  object ComposedShapeModel {
    def apply(x: Int, y: Int, maxSuppedDromCenter: Double, inner: ListBuffer[ModelElement], group: Int, speed: Speed = Speed.None): BaseModel = {
      BaseModel(BasePos(x, y), Shapes.ComposedShape(inner, maxSuppedDromCenter), group, speed)
    }

  }

  object RectangleModel {
    def apply(x: Int = 0, y: Int = 0, pos: Option[Pos] = None, w: Int = 1, h: Int = 1, group: Int, speed: Speed = Speed.None): BaseModel = {
      pos match {
        case Some(p) => BaseModel(p, Shapes.Rectangle(w, h), group, speed)
        case None => BaseModel(BasePos(x, y), Shapes.Rectangle(w, h), group, speed)
      }

    }
  }

  def apply(x: Int = 0, y: Int = 0, shape: Shape = Shapes.Point(5), pos: Option[Pos] = None, w: Int = 1, h: Int = 1, group: Int, speed: Speed = Speed.None): BaseModel = {
    pos match {
      case Some(p) => BaseModel(p, shape, group, speed)
      case None => BaseModel(BasePos(x, y), shape, group, speed)
    }

  }


  object CircleModel {
    def apply(x: Int, y: Int, r: Int, group: Int, speed: Speed = Speed.None): BaseModel = {
      BaseModel(BasePos(x, y), Shapes.Circle(r), group, speed)
    }
  }


  val y2 = BaseModel(BasePos(10, 7), Shapes.Circle(Color.decode("#FFFF00"))(7), Group.User)
  val headAll = BaseModel(Org, Shapes.Circle(Color.decode("#00FF00"))(30), Group.User)
  val y1 = y2.trX
  val l = ListBuffer[ModelElement](headAll, y1, y2)
  val headDraw = ComposedShapeModel(50, 50, 30, l, Group.User, Speed.None)
  val headImage = Shapes.Image("user", Direction.up, 20, 20)
  val headModel = Model(30, 30, headImage, speed = Speed.None, group = Group.User)
  val player1 = headModel
  val player2 = Model(30, 120, headImage, speed = Speed.None, group = Group.User)
  val Test: Model = {


    val el = mutable.ListBuffer[ModelElement](PlateauBase, RectangleModel(x = 200, y = 200, w = 20, h = 20, speed = Speed(3, 2), group = Group.Source)

    )
    new Model {
      override def elements = el

      override def player1: ModelElement = Model.player1

      override def player2: ModelElement = Model.player2
    }
  }
}













