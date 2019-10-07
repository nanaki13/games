package bon.jo.view


import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Font, Graphics, Graphics2D}
import java.awt.event.{KeyEvent, KeyListener}
import java.awt.geom.{AffineTransform, Ellipse2D, Rectangle2D}

import bon.jo.conf.Conf
import bon.jo.controller.ControllerMitron
import bon.jo.model.{MitronAthParam, Model}
import bon.jo.model.Model._
import bon.jo.model.Shape
import bon.jo.model.Shape.{Circle, ComposedShape, Point, Rectangle}
import bon.jo.model.Shapes.{DirAndIdParam, ShapeParamMultiple, ShapeParamOne, ShapeParamTwo}
import javax.swing.{JButton, JPanel, WindowConstants}

import scala.util.Random

class MitronAwtView(val elmts: Model, val controller: ControllerMitron) extends JPanel with AwtView[ MitronAthParam] with Refreh with KeyListener {



  import javax.imageio.ImageIO
  override val name= "Mirtou"
  val bullet = ImageIO.read(getClass.getResourceAsStream("/bullet.png"))
  val ennemi = ImageIO.read(getClass.getResourceAsStream("/Ships/Dove.png"))
  val user = ImageIO.read(getClass.getResourceAsStream("/Ships/Turtle.png"))
  override val image = user
  val user2 = ImageIO.read(getClass.getResourceAsStream("/Ships/Turtle.png"))
  val item = ImageIO.read(getClass.getResourceAsStream("/box/box.png"))
  val deltaStart = BasePos(-1, 1)
  var _athParam: MitronAthParam = MitronAthParam(0, 0, 0)


  def arhParam_=(arhParam: MitronAthParam) = {
    _athParam = arhParam

  }
  def arhParam = _athParam
  override def init() = {


    this.setPreferredSize(new Dimension(PlateauBase.w, PlateauBase.h))

    button.addActionListener(e => controller.newGame(1))
    button2.addActionListener(e => controller.newGame(2))
    south.setLayout(new FlowLayout())
    south.add(button)
    south.add(button2)
    root.add(south, BorderLayout.SOUTH)

    super.init()

  }




  val identity = new AffineTransform

  def drawImage(e: ModelElement, pa: DirAndIdParam)(implicit g2d: Graphics2D) = {
    var angle = pa.direction.toAngle

    while (e.speed.toAproDirection.toAngle != angle) {

      angle = (((angle + 90) + 360) % 360 - 180)
      angle = if (angle == -180) 180 else angle


    }
    import java.awt.geom.AffineTransform
    val trans = new AffineTransform

    trans.setTransform(identity)
    trans.translate(e.x, e.y)
    if (angle != 0) {
      trans.rotate(Math.toRadians(angle))
    }
    val img = pa.x match {
      case "bullet" => bullet
      case "enemy" => ennemi
      case "user" => user
      case "item" => item
    }
    trans.translate(-img.getWidth / 2, -img.getHeight / 2)


    g2d.drawImage(img, trans, this)

  }



  var ff = for {_ <- 1 to 50
  } yield {
    Pos.random(PlateauBase.w, PlateauBase.h)
  }

  def inPlateau(newPos: Pos): Boolean = {
    newPos.x >= 0 && newPos.x <= PlateauBase.w && newPos.y >= 0 && newPos.y <= PlateauBase.h
  }

  def stars(pos: Pos): Pos = {
    val newPos = pos + deltaStart
    if (inPlateau(newPos)) {
      newPos
    } else {
      if (Random.nextBoolean()) {
        BasePos(Random.nextInt(PlateauBase.w), 0)
      } else {
        BasePos(PlateauBase.w, Random.nextInt(PlateauBase.h))
      }

    }
  }

  override def paint(g: Graphics): Unit = { //custom color
    //create new Graphics2D instance using Graphics parent

    implicit val g2 = g.asInstanceOf[Graphics2D]

    //set color

    g2.clearRect(0, 0, PlateauBase.w, PlateauBase.h)
    //set thickness

    //draw the line, start x/y coords; end x/y coords;
    //it's a Double because you can be super-precise

    //set the thickness... increase the number and see what happens, e.g. 6f
    import java.awt.{Color, GradientPaint}

    g2.setPaint(Color.decode("#06445D"))
    g2.fillRect(PlateauBase.x, PlateauBase.y, PlateauBase.w, PlateauBase.h)


    g2.setPaint(Color.white)
    ff = ff.map(stars)
    ff.foreach(p => g2.drawOval(p.x, p.y, 2, 2))
    val _a = (PlateauBase.w / 1.5).toFloat
    val _b = PlateauBase.h.toFloat
    val a = _a * cntPlanetGrow / (1000f)
    val b = _b * cntPlanetGrow / (1000f)
    cntPlanetGrow += 1

    val apbs2 = (a + b) / 2
    val blackToGray = new GradientPaint(PlateauBase.w.toFloat - apbs2, PlateauBase.h.toFloat - apbs2, Color.BLACK, (PlateauBase.w / 1.5).toFloat, PlateauBase.h.toFloat, Color.decode("#CB7310"))
    g2.setPaint(blackToGray)
    val planet = new Ellipse2D.Float(PlateauBase.w.toFloat - apbs2, PlateauBase.h.toFloat - apbs2, (PlateauBase.w / 1.5).toFloat, PlateauBase.h.toFloat)
    g2.fill(planet)
    if (cntPlanetGrow > 3000) {
      cntPlanetGrow = 0
    }
    implicit val offset = BasePos(0, 0)
    //draw the line
    elmts.elements.foreach {
      draw
    }
    drawScore
    if (controller.pause && controller.gameOver) {
      val offset_ = BasePos(PlateauBase.w / 2, PlateauBase.h / 2)
      gameOver(g2, offX = offset_)
    }


  }

  private var cntPlanetGrow: Float = 0

  override def keyTyped(e: KeyEvent): Unit = {


  }

  override def keyPressed(e: KeyEvent): Unit = {
    e.getKeyCode match {
      case 40 => controller.userWant(Direction.down)
      case 38 => controller.userWant(Direction.up)
      case 37 => controller.userWant(Direction.right)
      case 39 => controller.userWant(Direction.left)
      case 90 => controller.userWant2(Direction.up)
      case 83 => controller.userWant2(Direction.down)
      case 81 => controller.userWant2(Direction.right)
      case 68 => controller.userWant2(Direction.left)
      case _ =>

    }
  }

  override def keyReleased(e: KeyEvent): Unit = {
    e.getKeyCode match {
      case 40 => controller.userWant(Direction.none)
      case 38 => controller.userWant(Direction.none)
      case 37 => controller.userWant(Direction.none)
      case 39 => controller.userWant(Direction.none)
      case 90 => controller.userWant2(Direction.none)
      case 83 => controller.userWant2(Direction.none)
      case 81 => controller.userWant2(Direction.none)
      case 68 => controller.userWant2(Direction.none)
      case 17 => controller.userWantShowt()
      case 16 => controller.userWantShowt()
      case 32 => controller.userWantShowt2()

      case e => println(s"${e} not mapped")
    }

  }




  def drawScore(implicit g2: Graphics2D): Unit = {
    g2.setPaint(Color.magenta)
    val fonte = new Font("TimesRoman ", Font.BOLD, 13);
    g2.setFont(fonte);

    g2.drawString(s"\nscore : ${_athParam.score}", 10, 15)
    g2.drawString(s"\nrecord : ${_athParam.maxScore}", 10, 15 + fonte.getSize + 2)
    g2.drawString(s"\nbullet : ${_athParam.nbBullet}", 10, 15 + (fonte.getSize + 2) * 2)
  }

  val south: JPanel = new JPanel()
  val button = new JButton("new Game(1J)")
  val button2 = new JButton("new Game(2J)")

  override def gameOver(implicit g2: Graphics2D, offX: Pos): Unit = {
    // g2.clearRect(0, 0, PlateauBase.w, PlateauBase.h)
    g2.setPaint(Color.black)
    val fonte = new Font("TimesRoman ", Font.BOLD, 30);
    g2.setFont(fonte);

    g2.drawString(s"Well Done : ${_athParam.score.toString}", offX.x - 10, offX.y - 10)


  }




}
