package bon.jo.conf

import bon.jo.model.Proba.CreatorShortcut._
import bon.jo.model.Proba.{ProbaEvent, ProbaEvolution}

import scala.language.postfixOps
object ConfDefault{
  val startProbaMonstre = (0.5, 100) ~
  val prod = false
}

case class ConfDefault(
                        debug: Boolean = false,
                        deltaTAnim: Int = 25,
                        url: String = if(ConfDefault.prod) "http://mitron-server.herokuapp.com/scores" else "http://mitron-server.herokuapp.com/scores",
                        serverPort: Int = if(ConfDefault.prod)  123 else 1234,
                        plateauSize: (Int, Int) = (1400, 1000),
                        nbBullet: Int = 10,
                        var enemyProba: ProbaEvent = ConfDefault.startProbaMonstre,
                        newBulletProba: ProbaEvent = (0.5, 100) ~,
                        newNoveProba: ProbaEvent = (0.2, 100) ~,
                        ennemyEvoution: ProbaEvolution = (0.7, 100) ev ( -1, 0.01),
                        outFile :  SerUNerOption = SerUNerOption("data.v1.1"),
                        oldOutFile :  SerUNerOption = SerUNerOption("data.v1.0")

               )