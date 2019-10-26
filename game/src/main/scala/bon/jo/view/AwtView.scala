package bon.jo.view

import java.awt.event.{ItemEvent, ItemListener, KeyEvent, KeyListener}
import java.awt.geom.{AffineTransform, Ellipse2D, Rectangle2D}
import java.awt._

import bon.jo.conf.Conf
import bon.jo.model.Model._
import bon.jo.model.Shape.{Circle, ComposedShape, Point, Rectangle}
import bon.jo.model.Shapes.{DirAndIdParam, ShapeParamMultiple, ShapeParamOne, ShapeParamTwo}
import bon.jo.model.{AthParam, Shape}
import java.awt.{Dimension, Toolkit}
import java.io.IOException
import java.net.URL

import bon.jo.controller.Controller

import scala.concurrent.ExecutionContext.Implicits._
import javax.swing.event.{HyperlinkEvent, HyperlinkListener}
import javax.swing.{JButton, JCheckBoxMenuItem, JEditorPane, JFrame, JLabel, JMenu, JMenuBar, JMenuItem, JOptionPane, JPanel, JTextArea, JTextPane, WindowConstants}

trait AwtView[AthParam_ <: AthParam] extends View[Graphics2D, AthParam_] with KeyListener {
  this: Component =>
  val frame = new JFrame(name)
  val borderLayout: BorderLayout = new BorderLayout
  val root: JPanel = new JPanel(borderLayout)
  val menuBar: JMenuBar = new JMenuBar
  frame.setJMenuBar(menuBar)
  val about = new EditorPaneLinkDetector()

  about.addHyperlinkListener((e: HyperlinkEvent) => {
    val clickedURL = e.getURL
    if (e.getEventType == HyperlinkEvent.EventType.ACTIVATED) try {
      fullScreenItem.setState(false)
      BrowserLauncher.openURL(clickedURL.toString)
    }
    catch {
      case e1: IOException =>
        e1.printStackTrace()
    }
  })
  //  about.setContentType("text/html");
  //
  //  about.setEditable(false);
  //  about.setBackground(null);
  //  about.setBorder(null);

  about.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, true);
  // about. setFont(DEFAULT_FONT);

  val gameMenu = new JMenu("Game")
  menuBar.add(gameMenu)
  val graphicMenu = new JMenu("Graphics")
  menuBar.add(graphicMenu)
  val helpMenu = new JMenu("Help")
  val aboutMenuItem = new JMenuItem("About")
  menuBar.add(helpMenu)
  helpMenu.add(aboutMenuItem)
  aboutMenuItem.addActionListener(l => {
    val f = new JFrame("About")
    f.getContentPane.add(about)

    f.setPreferredSize(new Dimension(300, 300))
    f.pack()
    f.setLocationRelativeTo(frame)
    f.setIconImage(image)
    f.setVisible(true)


  })

  val exit = new JMenuItem("Exit")
  exit.addActionListener(_ => System.exit(0))
  gameMenu.add(exit)
  val optoin = new JMenu("Option")
  graphicMenu.add(optoin)
  var fullScreen = true

  frame.setResizable(false)


  val screenSize: Dimension = Toolkit.getDefaultToolkit.getScreenSize
  val zoomFullScreen = screenSize.height / PlateauBase.h.toDouble
  frame.setUndecorated(true)
  frame.setExtendedState(Frame.MAXIMIZED_BOTH)
  var zoom: Double


  val zomms = Array(0.5, 0.7, 0.9, 1, 1.2, zoomFullScreen)
  val chAndZoom = zomms.map(scAndZoom)

  def scAndZoom(d: Double) = {
    (s"${(PlateauBase.w * d).round} * ${(PlateauBase.h * d).round}", d)
  }

  val fullScreenCheckRes = ni(scAndZoom(zoomFullScreen)._1)
  val alll = for {
    z <- chAndZoom
    i = if (z._2 == zoomFullScreen) {
      fullScreenCheckRes
    } else {
      ni(z._1)
    }
  } yield (i, z._2)
  val a: Map[JCheckBoxMenuItem, Double] = alll.map { e =>
    if (e._2 == zoomFullScreen) {
      e._1.setSelected(true)
    }
    e
  }.toMap
  val aa: Array[JCheckBoxMenuItem] = a.iterator.toList.sortBy(_._2).map(_._1).toArray
  val fullScreenItem = new JCheckBoxMenuItem("full screen")
  fullScreenItem.setState(fullScreen)

  val i: ItemListener = (e: ItemEvent) => {
    if (e.getStateChange == ItemEvent.SELECTED) {
      e.getItem match {
        case item: JCheckBoxMenuItem => {
          zoom = a(item)
        }
        case _ =>
      }
      aa.filter(item => item.getState && item != e.getItem).foreach(_.setState(false))
    }
  }

  aa.map(e => {
    e.addItemListener(i);
    e
  }).foreach(optoin.add)

  optoin.addSeparator
  fullScreenItem.addItemListener((e) => {
    fullScreen = e.getStateChange == ItemEvent.SELECTED
    if (!fullScreen) {
      packedOne = false
      frame.dispose()
      frame.setUndecorated(false)
      frame.setVisible(true)
      fullScreenCheckRes.setState(false)
      aa(0).setState(true)
    } else {
      frame.dispose()
      frame.setUndecorated(true)
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)
      frame.setVisible(true)
      fullScreenCheckRes.setState(true)

    }
  })

  optoin.add(fullScreenItem)
  optoin.addSeparator
  optoin.add(new JMenuItem("MasterQpuc spécial dédicace"))


  def ni(s: String): JCheckBoxMenuItem = new JCheckBoxMenuItem(s)

  def zoom(g: Graphics): AffineTransform = {
    val g2 = g.asInstanceOf[Graphics2D]


    val at = new AffineTransform
    if (fullScreen) {
      val screen = Toolkit.getDefaultToolkit.getScreenSize
      at.setToTranslation((screen.width - PlateauBase.w * zoom) / 2d, (screen.height - PlateauBase.h * zoom) / 2d)

    } else {
      if (!packedOne) {
        frame.pack()
        packedOne = true
      }
    }
    at.scale(zoom, zoom)


    g2.setTransform(at)
    at.createInverse()
  }

  def name: String

  def image: Image

  var packedOne = false

  override def init(): Unit = {
    root.add(this)
    root.addKeyListener(this)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.getContentPane.add(root)
    //frame.setLocation(550, 25)
    frame.setVisible(true)
    frame.setIconImage(image);

    // frame.pack()
    root.requestFocus()
  }

  override def refresh = {
    root.invalidate()
    root.repaint()
  }

  val userIn = new StringBuilder
  var register = false

  def getLoserUserName: Unit = {
    register = true
    userIn.setLength(0)
  }

  var cntInputText: Int = 1
  var userMessage: String

  def _keyTyped(e: KeyEvent): Unit = {

    if (register) {
      register = !(e.getKeyCode == 10 /* enter */)
      if (register) {
        val keyCode = e.getKeyCode

        if (keyCode >= KeyEvent.VK_0 && keyCode <= KeyEvent.VK_9 ||
          keyCode >= KeyEvent.VK_A && keyCode <= KeyEvent.VK_Z) {
          userIn.append(e.getKeyChar)
        } else if (e.getKeyCode == KeyEvent.VK_BACK_SPACE && !userIn.isEmpty) {
          userIn.setLength(userIn.length() - 1)
        }
      } else {

        controller.userName = userIn.toString()
        userIn.setLength(0)
        controller.continuRegisterUserName.map { e =>
          if (e == (true, "continu")) {
            cntInputText += 1
          } else {
            cntInputText = 1
          }
          register = e._1
          userMessage = e._2
          e
        }
      }
    }


  }

  def drawImage(e: ModelElement, pa: DirAndIdParam)(implicit g2d: Graphics2D)

  def draw(e: ModelElement)(implicit g2: Graphics2D, offX: Pos): Unit = {

    g2.setColor(e.awtColor)
    e match {
      case PlateauBase => g2.draw(new Rectangle2D.Double(PlateauBase.x, PlateauBase.y, PlateauBase.w, PlateauBase.h))
      case BaseModel(p, Shape(Point, pa: ShapeParamOne[_]), _, _, _, _) => pa.x match {
        case i: Int => g2.fill(new Rectangle2D.Double(p.x + offX.x - i / 2d, p.y + offX.y - i / 2d, i, i))
      }
      case BaseModel(p, Shape(Rectangle, pa: ShapeParamTwo), _, _, _, _) => g2.fill(new Rectangle2D.Double(p.x + offX.x - pa.x / 2d, p.y + offX.y - pa.y / 2d, pa.x, pa.y))
      case BaseModel(p, Shape(Circle, pa: ShapeParamOne[_]), _, _, _, _) => pa.x match {
        case int: Int => {
          val d = 2 * int

          g2.fill(new Ellipse2D.Double(p.x + offX.x - int, p.y + offX.y - int, d, d))
        }
      }
      case BaseModel(p, Shape(Shape.Image, pa: DirAndIdParam), gr, _, _, _) => {
        drawImage(e, pa)
//        if(gr == Group.User){
//          val pre = Controller.predict(e,PlateauBase,100)
//          drawImage(e._copy(pre), pa)
//        }
      }
      case BaseModel(p, Shape(ComposedShape, pa: ShapeParamMultiple), _, _, _, _) => {
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
          case Shape(Circle, pa: ShapeParamOne[_]) => {
            pa.x match {
              case r: Int => {
                val rD = 2 * r.toFloat
                val d = 2 * r

                val dist = Array(0.97f*(rD*rD*rD)/(702*702*702),0.98f*((rD*rD)/(702*702)),1.0f*(rD/702));
                val colors = Array( Color.decode("#e3381e"), Color.decode("#ba3622"),  Color.decode("#1f0d16"))
//              val colors = Array( Color.RED, Color.BLUE,  Color.GREEN)
                val blackToGray = new RadialGradientPaint(e.x + offX.x + offX.x , e.y + offX.y + offX.x ,r, dist, colors)
                g2.setPaint(blackToGray)
                g2.fill(new Ellipse2D.Double(e.x + offX.x - r, e.y + offX.y - r, d, d))
              }

            }

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
