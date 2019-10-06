package model

import model.Model.{ModelElement, Pos, Refreh}

trait View[G] extends Refreh {
  def newGame: Unit

  def draw(e: ModelElement)(implicit graphicContext: G, offX: Pos)

  def init()
  def score(score : Int,maxScore : Int,nbBullet : Int)
  def gameOver(implicit graphicContext: G, offX: Pos)
}
