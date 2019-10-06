package model

import java.io._

import model.Model.{player1, _}
import model.Shape.ComposedShape
import model.Shapes.ShapeParamMultiple
import model.Speed.builder

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

trait ControllerMitron extends Controller {


  override val model = Model.Test
  private var _pause = false
  private var _gameOver = false
  private var _nbJ: Int = 1
  private var _score = 0
  private var _cnt = 1
  private var _bulletCnt = Conf.nbBullet
  override var view: View[_] = null

  val sourceIndex = 1


  var maxScore = readMaxScore()

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

  def notifyViewATH = {
    view.score(_score, maxScore, _bulletCnt)
  }

  def nbJ = _nbJ

  def pause_=(pause: Boolean) = {
    _pause = pause
  }

  def randomPos = Pos.random(PlateauBase.w, PlateauBase.h)

  def safePos = {
    val ret = randomPos
    if (ret.near(model.elements(sourceIndex).pos, 150)) {
      randomPos
    } else {
      ret
    }
  }

  def newGame(nbJ: Short): Unit = {
    //  gameOver = false


    val jList = if (nbJ == 2) ListBuffer(player2) else Nil
    val el = ListBuffer(model.elements(0), model.elements(1)._copy(pos = BasePos(200, y = 200), speed = Speed(3, 2)), player1) ++ jList
    player1.pos = safePos
    player2.pos = safePos
    model.elements.clear()
    model.elements.addAll(el)

    view.newGame

    _score = 0
    notifyViewATH
    _bulletCnt = Conf.nbBullet
    _cnt = 1
    _pause = false
    _gameOver = false
    _nbJ = nbJ
    Conf.enemyProba = ConfDefault.startProbaMonstre

  }

  def pause: Boolean = _pause

  def gameOver: Boolean = _gameOver

  def update(e: (ModelElement, Int), elment: ListBuffer[ModelElement], p: Plateau): Unit = {

    if (e._1 != PlateauBase) {

      val speedX_ = if (e._1.x < 0 || e._1.x > p.w) -e._1.speed.x else e._1.speed.x
      val speedY_ = if (e._1.y < 0 || e._1.y > p.h) -e._1.speed.y else e._1.speed.y
      val x__ = e._1.x + speedX_
      val y__ = e._1.y + speedY_
      val x_ = if (x__ < 0) 0 else if (x__ > p.w) p.w else x__
      val y_ = if (y__ < 0) 0 else if (y__ > p.h) p.h else y__
      val speedX = if (x__ < 0 || x__ > p.w) -e._1.speed.x else e._1.speed.x
      val speedY = if (y__ < 0 || y__ > p.h) -e._1.speed.y else e._1.speed.y
      e._1.speed = Speed(speedX, speedY)
      e._1.pos = BasePos(x_, y_)
      //   elment.update(e._2, e._1._copy(BasePos(x_, y_), speed = e._1.speed.copy(speedX, speedY)))
      e._1 match {
        case BaseModel(p, Shape(ComposedShape, pa: ShapeParamMultiple), _, _, _) => {
          pa.inner.zipWithIndex.foreach(e => update(e, pa.inner, PlateauBase))
        }
        case _ =>
      }

    }

  }

  private def writeMaxScore(scoreMax: Int) = {

    val fw = new DataOutputStream(new FileOutputStream("data2"))
    fw.writeUTF("max_score")
    fw.writeInt(scoreMax)
    fw.close()
  }

  def readMaxScore(): Int = {
    if ((new File("data2")).exists()) {
      val fw = new DataInputStream(new FileInputStream("data2"))
      fw.readUTF()
      val ret = fw.readInt()
      fw.close()
      ret
    } else {
      0
    }
  }

  def players = player1 :: {
    if (_nbJ == 2) {
      List(player2)
    } else Nil
  }

  def isGameOver = {
    val isNear = for {
      e2 <- model.elements.filter(e => (e.group == Group.Enemy || e.group == Group.Source) && e != PlateauBase)
      e1 <- players
    } yield {
      e1.pos.near(e2.pos, math.max(e1.shape.getMaxNormFromCenter, e2.shape.getMaxNormFromCenter))
    }
    val n = isNear.isEmpty || isNear.max
    //if yes -> game over
    if (n) {
      _gameOver = true
      _pause = true

      if (_score > maxScore) {
        maxScore = _score
        writeMaxScore(maxScore)
      }
    }
  }

  def addNova(pos: Pos) = model.elements += new CircleGrow(pos, 350)

  def bulletProcess = {
    val tmpProcessBullet: mutable.Seq[List[Int]] = for {
      e1 <- model.elements.zipWithIndex.filter(e => (e._1.group == Group.Bullet) && e._1 != PlateauBase)
      e2 <- model.elements.zipWithIndex.filter(e => (e._1.group == Group.Enemy) && e._1 != PlateauBase)
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
          case e: NovaItem => addNova(e.pos)
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

  def newEnemy(pos: Pos) = {
    val speed = new Speed(Random.nextInt(9) - 4, Random.nextInt(9) - 4)
    model.elements.append(
      BaseModel(pos, shape = Shapes.Image("enemy", Direction.right, 20, 20), Group.Enemy, speed)
    )
    _score += 10
    notifyViewATH

  }


  def notPauseProcess = {

    var source_ : Pos = null
    model.elements.zipWithIndex.map(e => {
      update(e, model.elements, PlateauBase)
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
    isGameOver
    bulletProcess

    if (Conf.enemyProba.draw(_cnt)) {
      newEnemy(source_)
    }
    if (Conf.newBulletProba.draw(_cnt)) {
      model.elements += new BuletItem(Pos.random(PlateauBase.w, PlateauBase.h))
    }
    if (Conf.newNoveProba.draw(_cnt)) {
      model.elements += new NovaItem(Pos.random(PlateauBase.w, PlateauBase.h))
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


  def rem(indexToRemove: Iterable[Int]): Unit = for {
    i <- indexToRemove.toSet.toList.sorted.reverse
  } yield {
    model.elements.remove(i)
  }

  def userWant(direction: Direction): Unit = {
    direction match {
      case Direction.left => model.player1.speed = Speed(5, 0)
      case Direction.right => model.player1.speed = Speed(-5, 0)
      case Direction.down => model.player1.speed = Speed(0, 5)
      case Direction.up => model.player1.speed = Speed(0, -5)
      //  case Direction.none => player <-- Speed.None
      case _ =>
    }
  }

  def userWant2(direction: Direction): Unit = {
    direction match {
      case Direction.left => model.player2.speed = Speed(5, 0)
      case Direction.right => model.player2.speed = Speed(-5, 0)
      case Direction.down => model.player2.speed = Speed(0, 5)
      case Direction.up => model.player2.speed = Speed(0, -5)
      //  case Direction.none => player <-- Speed.None
      case _ =>
    }
  }


}

