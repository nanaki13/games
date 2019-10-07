package bon.jo.view

import bon.jo.model.AthParam
import bon.jo.model.Model.{ModelElement, Pos, Refreh}

trait View[G,AthParam_ <: AthParam] extends Refreh {

  var arhParam : AthParam_
  def newGame: Unit

  def draw(e: ModelElement)(implicit graphicContext: G, offX: Pos)

  def init()

  def gameOver(implicit graphicContext: G, offX: Pos)
}
