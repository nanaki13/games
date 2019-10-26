package bon.jo.main

import java.awt.{Graphics, Graphics2D, Image}
import java.awt.event.KeyEvent
import java.awt.geom.Ellipse2D

import bon.jo.controller.{Controller, ControllerMitron, ControllerMitronTestAth}
import bon.jo.model.Model.{BasePos, PlateauBase}
import bon.jo.model.{MitronAthParam, Model, Score, Shapes}
import bon.jo.view.{AwtView, MitronAwtView, View}
import javax.imageio.ImageIO
import javax.swing.JPanel

object MainTest{

   class ControllerTest extends ControllerMitronTestAth {
    override var view: View[_, MitronAthParam] = _
    override var pause: Boolean = false
     override var userName = ""
     override val model: Model = Model.Test

    override def notifyViewATH: Unit = {}

    override def notPauseProcess: Unit = {}

     override def continuRegisterUserName = ???

//     override def predict(me: Model.ModelElement, after: Int): Model.Pos = ???
   }
  class ViestTest(override val controller : ControllerTest) extends JPanel with AwtView[MitronAthParam] {
    override var userMessage : String = ""
    override def name: String = "test"

    override def image: Image = ImageIO.read(getClass.getResourceAsStream("/Ships/Turtle.png"))

    override def drawImage(e: Model.ModelElement, pa: Shapes.DirAndIdParam)(implicit g2d: Graphics2D): Unit ={}

    var _athParam: MitronAthParam = MitronAthParam(Score.None, Score.None, 0,0)


    def arhParam_=(arhParam: MitronAthParam) = {
      _athParam = arhParam

    }
    def arhParam = _athParam

    override def gameOver(implicit graphicContext: Graphics2D, offX: Model.Pos): Unit ={

    }

    override def keyTyped(e: KeyEvent): Unit = {}

    override def keyPressed(e: KeyEvent): Unit = {}

    override def keyReleased(e: KeyEvent): Unit =  {}
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



      implicit val offset = BasePos(0, 0)
      //draw the line
      controler.model.elements.foreach {
        draw
      }

      if (!controller.pause ) {
        val offset_ = BasePos(PlateauBase.w / 2, PlateauBase.h / 2)
        gameOver(g2, offX = offset_)
      }


    }

    override var zoom: Double = _


  }

  val start = System.currentTimeMillis()
  val controler =  new ControllerTest
  controler.launch(m=>new ViestTest(controler))

}
