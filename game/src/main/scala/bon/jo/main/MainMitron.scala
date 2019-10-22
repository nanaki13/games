package bon.jo.main

import bon.jo.controller.ControllerMitron
import bon.jo.view.MitronAwtView

object MainMitron extends App {
  try {
    object Controller extends ControllerMitron

    val start = System.currentTimeMillis()

    Controller.launch(new MitronAwtView(_, Controller))
  } catch {
    case e: Exception => e.printStackTrace()
  }


}
