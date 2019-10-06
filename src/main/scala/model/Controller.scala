package model

trait Controller {
  var view: View[_]
  var pause: Boolean

  def model: Model
  def afterLaunch(viewInit: Model => View[_]) = {
    view = viewInit(model)
    pause = true
    view.init()
    notifyViewATH
  }

  def notifyViewATH

  def notPauseProcess

  def launch(viewInit: Model => View[_]): Unit = {
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
