package bon.jo.controller

import bon.jo.conf.Conf
import bon.jo.model.Model._
import bon.jo.model.Shape.ComposedShape
import bon.jo.model.Shapes.ShapeParamMultiple
import bon.jo.model.{AthParam, Model, Shape, Speed}
import bon.jo.view.View

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import scala.concurrent.duration.Duration

trait Controller[athParam <: AthParam] {
  var userName: String

  var view: View[_, athParam]
  var pause: Boolean

  def model: Model

  def afterLaunch(viewInit: Model => View[_, athParam]): Unit = {
    view = viewInit(model)
    pause = true
    view.init()
    notifyViewATH()
  }

  def notifyViewATH(): Unit

  def continuRegisterUserName: Future[(Boolean, String)]

  def notPauseProcess(): Unit

  def launch(viewInit: Model => View[_, athParam]): Unit = {
    afterLaunch(viewInit)
    synchronized {
      while (true) {
        //    val start = System.currentTimeMillis()
        if (!pause) {
          Future {
            notPauseProcess()
          }
        }
        //println(Duration(System.currentTimeMillis() - start, "ms"))
        wait(Conf.deltaTAnim)
        //println(Duration(System.currentTimeMillis() - start, "ms"))
        view.refresh
      }
    }
  }

}

object Controller {

  def predict(me: Model.ModelElement, pl: Plateau, after: Int): Pos = {
    predict(me.pos, me.speed, pl, after - 1)
  }
  def updatePosAndSpeed(e: ModelElement, p: Plateau): Unit = {
    e match {
      case s: SelfControl => s.updatePos()
      case _ => {
        val (pos, speed) = calcPosAndSpeed(e.pos, e.speed, p: Plateau)
        e.speed = speed
        e.pos = pos
      }
    }

  }
  def predict(pos: Pos, speed: Speed, pl: Plateau, after: Int): Pos = {
    if (after == 0) {
      pos
    } else {
      val (p, s) = calcPosAndSpeed(pos, speed, pl)
      predict(p, s, pl, after - 1)
    }

  }

  def calcPosAndSpeed(pos: Pos, speed: Speed, p: Plateau): (Pos, Speed) = {
    val speedX_ = if (pos.x < 0 || pos.x > p.w) -speed.x else speed.x
    val speedY_ = if (pos.y < 0 || pos.y > p.h) -speed.y else speed.y
    val x__ = pos.x + speedX_
    val y__ = pos.y + speedY_
    val x = if (x__ < 0) 0 else if (x__ > p.w) p.w else x__
    val y = if (y__ < 0) 0 else if (y__ > p.h) p.h else y__
    val vx = if (x__ < 0 || x__ > p.w) -speed.x else speed.x
    val vy = if (y__ < 0 || y__ > p.h) -speed.y else speed.y
    (BasePos(x, y), Speed(vx, vy))
  }
  def update(e: ModelElement, plat: Plateau): Unit = {
    if (e != plat) {
      updatePosAndSpeed(e, plat)
      e match {
        case BaseModel(p, Shape(ComposedShape, pa: ShapeParamMultiple), _, _,_, _) => {
          pa.inner.foreach(e => update(e, plat))
        }
        case _ =>
      }

    }

  }
}