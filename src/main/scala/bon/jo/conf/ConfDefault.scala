package bon.jo.conf

import bon.jo.model.Proba.CreatorShortcut._
import bon.jo.model.Proba.{ProbaEvent, ProbaEvolution}

import scala.language.postfixOps
object ConfDefault{
  val startProbaMonstre = (0.5, 100) ~
}

case class ConfDefault(
                        debug: Boolean = false,
                        deltaTAnim: Int = 25,
                        plateauSize: (Int, Int) = (1400, 1000),
                        nbBullet: Int = 10,
                        var enemyProba: ProbaEvent = ConfDefault.startProbaMonstre,
                        newBulletProba: ProbaEvent = (0.5, 100) ~,
                        newNoveProba: ProbaEvent = (0.2, 100) ~,
                        ennemyEvoution: ProbaEvolution = (0.7, 100) ev ( -1, 0.01)

               )