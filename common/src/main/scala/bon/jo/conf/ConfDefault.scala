package bon.jo.conf

import bon.jo.model.Proba.CreatorShortcut._
import bon.jo.model.Proba.{ProbaEvent, ProbaEvolution}

import scala.language.postfixOps
object ConfDefault{
  val startProbaMonstre = (0.7, 100) ~
  val prod = true
}

case class ConfDefault(
                        debug: Boolean = false,
                        inv: Boolean = false,
                        deltaTAnim: Int = 1000/50,
                        url: String = if(ConfDefault.prod) "https://mitron-server.herokuapp.com/scores" else "http://localhost:8080/scores",
                        serverPort: Int = if(ConfDefault.prod)  123 else 1234,
                        plateauSize: (Int, Int) = (1400, 1000),
                        nbBullet: Int = 10,
                        var enemyProba: ProbaEvent = ConfDefault.startProbaMonstre,
                        newBulletProba: ProbaEvent = (0.6, 100) ~,
                        newBulletEnnemyProba: ProbaEvent = (0.4, 150) ~,
                        newNoveProba: ProbaEvent = (0.21, 100) ~,
                        ennemyEvoution: ProbaEvolution = (0.7, 100) ev ( -0.5, 0.095),
                        outFile :  SerUNerOption = SerUNerOption("data.v1.1"),
                        oldOutFile :  SerUNerOption = SerUNerOption("data.v1.0"),
                        version : String = "1.2"

               )