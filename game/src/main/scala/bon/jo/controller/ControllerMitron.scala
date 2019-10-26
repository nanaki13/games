package bon.jo.controller

import java.time.LocalDate


import bon.jo.conf.{Conf, ConfDefault, SerUNerOption, SerUnserUtil}
import bon.jo.controller.ControllerMitron._
import bon.jo.model.Model._
import bon.jo.model.Shape.ComposedShape
import bon.jo.model.Shapes.ShapeParamMultiple
import bon.jo.model._
import bon.jo.network.MitronClient
import bon.jo.view.View

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import bon.jo.model.validator.Validator._

object ControllerMitron {
  implicit val game: String = "mitron_v1.0"
  val gameText: String = "Mitron"
}

sealed trait UserPower

object NovaPortable extends UserPower

trait ControllerMitron extends Controller[MitronAthParam] {

  override val model: Model = Model.Test
  val sourceIndex = 1
  private val _client = MitronClient
  override var view: View[_, MitronAthParam] = null
  var maxScore: Score = Score.None.copy()
  var _online: Boolean = false
  private var _pause = false
  private var _gameOver = false
  private var _nbJ: Int = 1
  private var _score = Score.None.copy()
  private var _cnt = 1
  private var _scores: Scores = Scores.empty
  private var _onlineScores: Scores = Scores.empty
  private var _bulletCnt = Conf.nbBullet
  private var _userName: String = ""
  private var wantStopWithoutRegsiter: Boolean = true

  def userWantNova(): Unit = {
    if (playerHaveNova) addNova(model.player1)
  }

  def user2WantNova(): Unit = {
    if (player2HaveNova) addNova(model.player1)
  }

  def addNova(pos: Pos) = model.elements += new CircleGrow(pos, 350)

  def scores = _scores

  def onlineScores = _onlineScores

  def bestScoreListeLocal(implicit nbPlayer: Int, game: String): Seq[Score] = _scores.bestScoreListe

  def bestScoreListeOnline(implicit nbPlayer: Int, game: String): Seq[Score] = _onlineScores.bestScoreListe

  def playerHaveNova = model.player1Power.contains(NovaPortable)

  def player2HaveNova = model.player2Power.contains(NovaPortable)

  def userName: String = {
    _userName
  }

  def userName_=(str: String): Unit = {
    if (!str.isBlank) {
      _userName = str
      _score = _score.copy(who = str :: _score.who, game = game)
      wantStopWithoutRegsiter = false
    } else {
      wantStopWithoutRegsiter = true
    }
  }

  def continuRegisterUserName: Future[(Boolean, String)] = {
    if (_score.who.size == _nbJ) {
      writeMaxScore(_score).map(e => (!e._1, e._2))
      // notifyViewATH
    } else if (wantStopWithoutRegsiter) {
      Future.successful(false, "Top score ignoré")
    } else {
      Future.successful(_score.who.size < _nbJ, "continu")
    }

  }

  private def writeMaxScore(scoreMax: Score): Future[(Boolean, String)] = {
    implicit val nbJ = _nbJ
    implicit val sb = new mutable.StringBuilder()
    if (valid(scoreMax)) {
      _scores.add(scoreMax)
      _scores.reduce
      SerUnserUtil.writeObject(_scores)
      val ret: Future[(Boolean, String)] = if (_online && (scoreMax > _onlineScores.min || _onlineScores.size < 20)) {
        val resp: Future[(Boolean, String)] = _client.writeScore(scoreMax).map { e =>
          if (e == "Ok") {
            (true, "Score send")
          } else {
            _score = _score.copy(who = Nil)
            (true, "Score not send : " + e)
          }

        }
        resp
      } else {
        Future.successful((false, "T'es pas dans le top 20"))
      }
      ret
    } else {
      _score = _score.copy(who = Nil)
      Future.successful((false, sb.toString()))
    }


  }

  override def afterLaunch(viewInit: Model => View[_, MitronAthParam]) = {
    maxScore = readLocalAndGetMax()
    _score = Score.None.copy(game)

    connect
    super.afterLaunch(viewInit)
  }

  def connect = {
    if (_online == false) {
      try {

        _client.getMaxScores.map {
          e => {
            _onlineScores = e
            _online = true
          }
        }

      } catch {
        case e: Exception => {
          e.printStackTrace();
          _online = false
        }
      }
    }
  }

  var _novatCnt = 0

  def addNovaP() = {
    _novatCnt += 1;
    notifyViewATH
  }

  def addBullet() = {
    _bulletCnt += 1;
    notifyViewATH
  }

  def userWantShowt(): Unit = {
    if (_bulletCnt > 0) {
      model.elements.append(
        new Buulet(pos = player1.pos,
          speed = player1.speed * 1.1f)
      )
      _bulletCnt -= 1
      notifyViewATH
    }

  }

  def userWantShowt2(): Unit = {
    if (_bulletCnt > 0) {
      model.elements.append(
        new Buulet(pos = player2.pos,
          speed = player2.speed * 1.1f)
      )
      _bulletCnt -= 1
      notifyViewATH
    }

  }

  def nbJ = _nbJ

  def newGame(implicit nbJ: Short): Unit = {
    //  gameOver = false


    val jList = if (nbJ == 2) ListBuffer(player2) else Nil
    val el = ListBuffer(model.elements(0), model.elements(1)._copy(pos = BasePos(200, y = 200), speed = Speed(3, 2)), player1) ++ jList
    player1.pos = safePos
    player2.pos = safePos
    model.elements.clear()
    model.elements.addAll(el)
    view.newGame
    _score = Score.None


    _bulletCnt = Conf.nbBullet
    _cnt = 1
    _pause = false
    _gameOver = false
    _nbJ = nbJ
    maxScore = readLocalAndGetMax()
    notifyViewATH
    Conf.enemyProba = ConfDefault.startProbaMonstre

  }

  def notifyViewATH = {
    view.arhParam = MitronAthParam(_score, maxScore, _bulletCnt, _novatCnt)
  }

  def safePos = {
    val ret = randomPos
    if (ret.near(model.elements(sourceIndex).pos, 150)) {
      randomPos
    } else {
      ret
    }
  }

  def randomPos = Pos.random(PlateauBase.w, PlateauBase.h)

  def readLocalAndGetMax(): Score = {
    implicit val nbJ = _nbJ
    _scores = SerUnserUtil.readObject(Scores.empty)
    if (_scores.scores.isEmpty) {
      Score.None.copy()
    } else {
      _scores.max
    }


  }


  def pause: Boolean = _pause


  implicit val optionFileee: SerUNerOption = Conf.outFile

  implicit def i18n(str: String): String = str

  def pause_=(pause: Boolean) = {
    _pause = pause
  }

  def gameOver: Boolean = _gameOver


  def readOnlie(): Unit = {

    _client.getMaxScores.map {
      e => {
        _onlineScores = e
        _online = true
      }
    }


  }

  def players = player1 :: {
    if (_nbJ == 2) {
      List(player2)
    } else Nil
  }

  def isGameOver = {
    val isNear = for {
      e2 <- model.elements.filter(e => (e.group == Group.Enemy || e.group == Group.Source || e.group == Group.BulletToPlayer) && e != PlateauBase)
      e1 <- players
    } yield {
      e1.pos.near(e2.pos, math.max(e1.shape.getMaxNormFromCenter, e2.shape.getMaxNormFromCenter))
    }
    val n = isNear.isEmpty || isNear.max
    //if yes -> game over
    if (n) {
      _gameOver = true
      _pause = true
      implicit val nbJ = _nbJ
      if (_score > maxScore || scores.size < 10) {
        _score = _score.copy(who = Nil, when = LocalDate.now())
        view.getLoserUserName
      }
    }
  }

  def bulletProcess = {
    val tmpProcessBullet: mutable.Seq[List[Int]] = for {
      e1 <- model.elements.zipWithIndex.filter(e => (e._1.group == Group.Bullet) && e._1 != PlateauBase)
      e2 <- model.elements.zipWithIndex.filter(e => (e._1.group == Group.Enemy || e._1.group == Group.BulletToPlayer) && e._1 != PlateauBase)
    } yield {
      if (e1._1.near(e2._1.pos, math.max(e1._1.shape.getMaxNormFromCenter, e2._1.shape.getMaxNormFromCenter))) {
        e1._1 match {
          case _: CircleGrow => {
            List(e2._2)
          }
          case e => {
            List(e1._2, e2._2)
          }
        }
      } else {
        Nil
      }
    }

    val elAndOoul: mutable.Seq[Int] = tmpProcessBullet.flatten


    _score += elAndOoul.size * 10
    if (!elAndOoul.isEmpty) {
      notifyViewATH
    }
    val tmpProcessUser = for {
      e1 <- model.elements.zipWithIndex.filter(e => (e._1.group == Group.Item) && e._1 != PlateauBase)
      e2 <- players
    } yield {
      if (e1._1.near(e2.pos, math.max(e1._1.shape.getMaxNormFromCenter, e2.shape.getMaxNormFromCenter))) {
        e1._1 match {
          case e: NovaItem if !playerHaveNova && !playerHaveNova => addNova(e.pos)
          case e: NovaItem if playerHaveNova || playerHaveNova => addNovaP()
          case a => addBullet()
        }
        Option(e1._2)
      } else {
        None
      }
    }


    val itemUser: mutable.Seq[Int] = tmpProcessUser.flatten


    rem(elAndOoul ++ itemUser)
  }

  val imageEnmmyId = List(Ennemy("enemy"), Ennemy("enemy1"), Ennemy("enemy2", CanShot))

  def newEnemy(pos: Pos) = {
    val ennemyRandom = imageEnmmyId(Random.nextInt(3))
    val speed = new Speed(Random.nextInt(9) - 4, Random.nextInt(9) - 4)
    model.elements.append(
      BaseModel(pos, shape = Shapes.Image(ennemyRandom.id, Direction.right, 20, 20), group = Group.Enemy, speed = speed, powers = ennemyRandom.powers)
    )
    _score += 10
    notifyViewATH

  }

  val startScoreNovaPortable = 1500

  def notPauseProcess = {

    var source_ : Pos = null
    model.elements.zipWithIndex.map(e => {
      Controller.update(e._1, PlateauBase)
      e
    })
      .map({ e =>
        e._2 match {
          case this.`sourceIndex` => {
            source_ = e._1.pos
            e
          }
          case _ => e
        }

      })
    //          updateUserCoord
    //Search if enemy near user
    if (!Conf.inv) isGameOver
    bulletProcess

    if (_score.value > startScoreNovaPortable) {
      model.player1Power = NovaPortable :: model.player1Power
      model.player2Power = NovaPortable :: model.player2Power
    }
    if (Conf.enemyProba.draw(_cnt)) {
      newEnemy(source_)
    }
    if (Conf.newBulletProba.draw(_cnt)) {
      model.elements += new BuletItem(Pos.random(PlateauBase.w, PlateauBase.h))
    }
    if (Conf.newNoveProba.draw(_cnt)) {
      model.elements += new NovaItem(Pos.random(PlateauBase.w, PlateauBase.h))
    }

    model.elements.filter(_.powers.contains(CanShot)) foreach { e =>
      Conf.newBulletEnnemyProba.drawAndDo(_cnt) {
        addHomingBullet(e)
      }
    }


    Conf.ennemyEvoution.draw(_cnt)(Conf.enemyProba) match {
      case Some(p) => {

        Conf.enemyProba = p

      }
      case _ =>
    }

    //remove die by time element
    rem(model.elements.zipWithIndex.filter(e => e._1.canDie && e._1.countBeforeDi == 0).map(_._2))
    _cnt += 1


  }

  def randomPlayer: ModelElement = if (_nbJ == 1) {
    player1
  } else if (Random.nextBoolean()) player1 else player2

  def addHomingBullet(e: ModelElement)(): Unit = model.elements += new HomingBullet(e.pos, randomPlayer)

  def rem(indexToRemove: Iterable[Int]): Unit = for {
    i <- indexToRemove.toSet.toList.sorted.reverse
  } yield {
    model.elements.remove(i)
  }

  def userWant(direction: Direction): Unit = {
    direction match {
      case Direction.left => model.player1.speed = Speed(-5, 0)
      case Direction.right => model.player1.speed = Speed(5, 0)
      case Direction.down => model.player1.speed = Speed(0, 5)
      case Direction.up => model.player1.speed = Speed(0, -5)
      //  case Direction.none => player <-- Speed.None
      case _ =>
    }
  }

  def userWant2(direction: Direction): Unit = {
    direction match {
      case Direction.left => model.player2.speed = Speed(-5, 0)
      case Direction.right => model.player2.speed = Speed(5, 0)
      case Direction.down => model.player2.speed = Speed(0, 5)
      case Direction.up => model.player2.speed = Speed(0, -5)
      //  case Direction.none => player <-- Speed.None
      case _ =>
    }
  }


}


