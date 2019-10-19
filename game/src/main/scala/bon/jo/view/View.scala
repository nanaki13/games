package bon.jo.view

import bon.jo.controller.Controller
import bon.jo.model.AthParam
import bon.jo.model.Model.{ModelElement, Pos, Refreh}

trait View[G,AthParam_ <: AthParam] extends Refreh {
  def getLoserUserName: Unit

  def controller : Controller[AthParam_]

  var arhParam : AthParam_
  def newGame: Unit

  def draw(e: ModelElement)(implicit graphicContext: G, offX: Pos)

  def init()

  def gameOver(implicit graphicContext: G, offX: Pos)
}
