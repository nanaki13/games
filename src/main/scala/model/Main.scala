package model

object Main extends App {

  object Controller extends ControllerMitron

  val start = System.currentTimeMillis()
  Controller.launch(new AwtView(_, Controller))

}
