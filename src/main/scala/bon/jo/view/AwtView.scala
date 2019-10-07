package bon.jo.view

import java.awt.event.KeyListener
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{BorderLayout, Color, Component, Graphics2D, Image}

import bon.jo.conf.Conf
import bon.jo.model.{AthParam, Shape}
import bon.jo.model.Model.{BaseModel, BasePos, ModelElement, PlateauBase, Pos}
import bon.jo.model.Shape.{Circle, ComposedShape, Point, Rectangle}
import bon.jo.model.Shapes.{DirAndIdParam, ShapeParamMultiple, ShapeParamOne, ShapeParamTwo}
import javax.swing.{JFrame, JPanel, WindowConstants}

trait AwtView[AthParam_ <: AthParam] extends View[Graphics2D, AthParam_] with KeyListener{
  this: Component =>
  val frame = new JFrame(name)
  val borderLayout: BorderLayout = new BorderLayout
  val root: JPanel = new JPanel(borderLayout)
  def name : String
  def image: Image
  override def init(): Unit = {
    root.add(this)
    root.addKeyListener(this)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.getContentPane.add(root)
    frame.setLocation(550, 25)
    frame.setVisible(true)
    frame.setIconImage(image);
    frame.pack()
    root.requestFocus()
  }
  override def refresh = {
    root.invalidate()
    root.repaint()
  }

  def drawImage(e: ModelElement, pa: DirAndIdParam)(implicit g2d: Graphics2D)

  def draw(e: ModelElement)(implicit g2: Graphics2D, offX: Pos): Unit = {
    g2.setColor(e.awtColor)
    e match {
      case PlateauBase => g2.draw(new Rectangle2D.Double(PlateauBase.x, PlateauBase.y, PlateauBase.w, PlateauBase.h))
      case BaseModel(p, Shape(Point, pa: ShapeParamOne[_]), _, _, _) => pa.x match {
        case i: Int => g2.fill(new Rectangle2D.Double(p.x + offX.x - i / 2d, p.y + offX.y - i / 2d, i, i))
      }
      case BaseModel(p, Shape(Rectangle, pa: ShapeParamTwo), _, _, _) => g2.fill(new Rectangle2D.Double(p.x + offX.x - pa.x / 2d, p.y + offX.y - pa.y / 2d, pa.x, pa.y))
      case BaseModel(p, Shape(Circle, pa: ShapeParamOne[_]), _, _, _)=> pa.x match {
        case int: Int => {
          val d = 2 * int
          g2.fill(new Ellipse2D.Double(p.x + offX.x - int, p.y + offX.y -int, d, d))
        }
      }
      case BaseModel(p, Shape(Shape.Image, pa: DirAndIdParam), _, _, _) => {
        drawImage(e, pa)
      }
      case BaseModel(p, Shape(ComposedShape, pa: ShapeParamMultiple), _, _, _) => {
        pa.inner.foreach { e =>
          implicit val offset = BasePos(p.x, p.y)
          draw(e)
        }
      }
      case e: ModelElement => {
        e.shape match {
          case Shape(Rectangle, pa: ShapeParamTwo) => {
            g2.fill(new Rectangle2D.Double(e.x + offX.x - pa.x / 2d, e.y + offX.y - pa.y / 2d, pa.x, pa.y))
          }
          case Shape(Circle, pa: ShapeParamOne[Int]) => {
            val d = 2 * pa.x;
            g2.fill(new Ellipse2D.Double(e.x + offX.x - pa.x, e.y + offX.y - pa.x, d, d))
          }
          case Shape(ComposedShape, pa: ShapeParamMultiple) => {
            pa.inner.foreach { e =>
              implicit val offset = BasePos(e.x, e.y)
              draw(e)
            }
          }
          case e => println(e)
        }

      }
      case e => println(e)


    }
    if (Conf.debug) {
      g2.setColor(Color.black)
      val pa = e.shape.getMaxNormFromCenter
      val d = 2 * pa

      g2.draw(new Ellipse2D.Double(e.x + offX.x - pa, e.y + offX.y - pa, d, d))
      g2.drawOval(e.x, e.y, 3, 3)
    }
  }
  override def newGame: Unit = {

    root.requestFocus()
  }
}
