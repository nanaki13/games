package bon.jo.controller

import bon.jo.conf.Conf
import bon.jo.model.{AthParam, Model}
import bon.jo.view.View

trait Controller[athParam <: AthParam] {
  var userName :String

  var view:  View[_,athParam]
  var pause: Boolean

  def model: Model
  def afterLaunch(viewInit: Model => View[_,athParam]) = {
    view = viewInit(model)
    pause = true
    view.init()
    notifyViewATH
  }

  def notifyViewATH
  def continuRegisterUserName : Boolean
  def notPauseProcess

  def launch(viewInit: Model => View[_,athParam]): Unit = {
    afterLaunch(viewInit)
    synchronized {
      while (true) {
        val start = System.currentTimeMillis()
        if (!pause) {
          notPauseProcess
        }
        wait(Conf.deltaTAnim)
        view.refresh
      }
    }
  }

}
