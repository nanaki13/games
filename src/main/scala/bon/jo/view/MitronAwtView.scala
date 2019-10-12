package bon.jo.view


import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Font, Graphics, Graphics2D, Paint}
import java.awt.event.{KeyEvent, KeyListener}
import java.awt.geom.{AffineTransform, Ellipse2D, Rectangle2D}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream, ObjectInputStream, ObjectOutputStream}
import java.net.{ServerSocket, Socket}
import java.time.Instant

import bon.jo.conf.Conf
import bon.jo.controller.Score.ScoreTest
import bon.jo.controller.{ControllerMitron, Score, Scores, SerUNerOption, SerUnserUtil}
import bon.jo.model.{MitronAthParam, Model}
import bon.jo.model.Model._
import bon.jo.model.Shape
import bon.jo.model.Shape.{Circle, ComposedShape, Point, Rectangle}
import bon.jo.model.Shapes.{DirAndIdParam, ShapeParamMultiple, ShapeParamOne, ShapeParamTwo}
import javax.swing.{JButton, JPanel, WindowConstants}
import bon.jo.controller.ControllerMitron.game

import scala.concurrent.Future
import scala.util.Random

class MitronAwtView(val elmts: Model, val controller: ControllerMitron) extends JPanel with AwtView[MitronAthParam] with Refreh with KeyListener {


  import javax.imageio.ImageIO

  override val name = "Mirtou"
  val bullet = ImageIO.read(getClass.getResourceAsStream("/bullet.png"))
  val ennemi = ImageIO.read(getClass.getResourceAsStream("/Ships/Dove.png"))
  val user = ImageIO.read(getClass.getResourceAsStream("/Ships/Turtle.png"))
  override val image = user
  val user2 = ImageIO.read(getClass.getResourceAsStream("/Ships/Turtle.png"))
  val item = ImageIO.read(getClass.getResourceAsStream("/box/box.png"))
  val deltaStart = BasePos(-1, 1)
  var _athParam: MitronAthParam = MitronAthParam.None


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

  def randomColor: Paint = new Color(Random.nextFloat(), Random.nextFloat(), Random.nextFloat())

  override def paint(g: Graphics): Unit = { //custom color
    //create new Graphics2D instance using Graphics parent

    implicit val g2 = g.asInstanceOf[Graphics2D]
    implicit val nb : Int = controller.nbJ

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
    ff.foreach(p => {
      if (register) {
        g2.setPaint(randomColor)
      }
      g2.drawOval(p.x, p.y, 2, 2)
    })
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
    drawATH
    if (controller.pause && controller.gameOver) {
      val offset_ = BasePos(PlateauBase.w / 2  - 100, (PlateauBase.h / 10))
      gameOver(g2, offX = offset_)
    }

    if (register || controller.pause) {
      g2.setFont(fonte)
      if (register) {
        val str = userIn.toString()
        g2.setPaint(defTexColor)
        g2.drawString(s"Enter your name J${cntInputText} : " + str, 200, 200)
      }

      val l = controller.bestScoreListeLocal
      val fullsize = l.size
      val brn = if (fullsize > 10) 10 else fullsize
      g2.drawString("Local", 200, 300)
      for (i <- 0 until brn) {
        g2.drawString(l(i).tuUiString, 200, 350 + i * 40)
      }
      if (controller._online) {
        g2.drawString("Online", 600, 300)
        val l = controller.bestScoreListeOnline
        val fullsize = l.size
        val brn = if (fullsize > 10) 10 else fullsize
        for (i <- 0 until brn) {
          g2.drawString(l(i).tuUiString, 600, 350 + i *40)
        }
      }else{
        g2.drawString("No connection", 600, 300)
      }
    }


  }

  private var cntPlanetGrow: Float = 0

  type View = AwtView[MitronAthParam]

  override def keyTyped(e: KeyEvent): Unit = {


  }

  override def keyPressed(e: KeyEvent): Unit = {
    if (!register) {
      e.getKeyCode match {
        case KeyEvent.VK_DOWN => controller.userWant(Direction.down)
        case  KeyEvent.VK_UP => controller.userWant(Direction.up)
        case  KeyEvent.VK_RIGHT => controller.userWant(Direction.right)
        case  KeyEvent.VK_LEFT => controller.userWant(Direction.left)
        case KeyEvent.VK_Z if controller.nbJ > 1=> controller.userWant2(Direction.up)
        case KeyEvent.VK_S if controller.nbJ > 1 => controller.userWant2(Direction.down)
        case KeyEvent.VK_D if controller.nbJ > 1=> controller.userWant2(Direction.right)
        case KeyEvent.VK_Q if controller.nbJ > 1=> controller.userWant2(Direction.left)
        case a => println(s"${a} not mapped")
      }
    }
  }

  override def keyReleased(e: KeyEvent): Unit = {
    if (!register) {
      e.getKeyCode match {
        case KeyEvent.VK_DOWN => controller.userWant(Direction.none)
        case  KeyEvent.VK_UP => controller.userWant(Direction.none)
        case  KeyEvent.VK_RIGHT => controller.userWant(Direction.none)
        case  KeyEvent.VK_LEFT => controller.userWant(Direction.none)
        case KeyEvent.VK_Z if controller.nbJ > 1=> controller.userWant2(Direction.none)
        case KeyEvent.VK_S if controller.nbJ > 1=> controller.userWant2(Direction.none)
        case KeyEvent.VK_D if controller.nbJ > 1=> controller.userWant2(Direction.none)
        case KeyEvent.VK_Q if controller.nbJ > 1 => controller.userWant2(Direction.none)
        case KeyEvent.VK_SHIFT => controller.userWantShowt()
        case KeyEvent.VK_CONTROL => controller.userWantShowt()
        case KeyEvent.VK_SPACE if(controller.nbJ > 1) => controller.userWantShowt2()
        case ee => {
          println(s"$ee not mapped")
        }
      }
    } else {
      _keyTyped(e)
      _athParam = _athParam.copy(maxScore = _athParam.score.copy(who = List(userIn.toString())))
    }


  }


  val defTexColor: Paint = Color.magenta
  val fonte = new Font("TimesRoman ", Font.BOLD, 30)
  val fonteMini = new Font("TimesRoman ", Font.BOLD, 13)
  def drawATH(implicit g2: Graphics2D): Unit = {
    g2.setPaint(defTexColor)
    g2.setFont(fonteMini);

    g2.drawString(s"\nscore : ${_athParam.score.value}", 10, 15)
    g2.drawString(s"\nrecord : ${_athParam.maxScore.value}     ${if (_athParam.maxScore != Score.None) s"[${_athParam.maxScore.who.mkString(" & ")}]" else ""}", 10, 15 + fonteMini.getSize + 2)
    g2.drawString(s"\nbullet : ${_athParam.nbBullet}", 10, 15 + (fonteMini.getSize + 2) * 2)
  }

  val south: JPanel = new JPanel()
  val button = new JButton("new Game(1J)")
  val button2 = new JButton("new Game(2J)")

  override def gameOver(implicit g2: Graphics2D, offX: Pos): Unit = {
    // g2.clearRect(0, 0, PlateauBase.w, PlateauBase.h)

    g2.setFont(fonte);

    g2.drawString(s"Well Done : ${_athParam.score.value}", offX.x - 10, offX.y - 10)


  }


}

















