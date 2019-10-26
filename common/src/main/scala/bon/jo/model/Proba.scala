package bon.jo.model

import scala.language.postfixOps

object Proba {

  trait Proba {
    def double: Double

    def draw: Boolean

  }

  sealed trait ProbaEvolution extends ProbaAndDo[ProbaEvent, ProbaEvent] {
    override def doIfOK(p: ProbaEvent): ProbaEvent = {

      val np = if (p.periodNb + dCount > 0) p.periodNb + dCount else 1
      val ret = ProbaEventImpl(p.proba.double + dProba, np)
      ret
    }

    def dCount: Double

    def dProba: Double
  }

  class ProbaEvolutionImpl(proba: Proba,
                           periodNb: Int, override val dCount: Double, override val dProba: Double) extends ProbaEventImpl(proba, periodNb) with ProbaEvolution

  sealed trait ProbaAndDo[-P, +R] extends ProbaEventBase[Proba] {

    def doIfOK(p: P): R

    def draw(cntPerdiod: Int)(p: P): Option[R] = {
      if (super.draw(cntPerdiod)) {
        Some(doIfOK(p))
      } else {
        None
      }
    }
  }

  case class ProbaImpl(double: Double) extends Proba {
    override def draw: Boolean = scala.math.random() < double
  }

  object CreatorShortcut {

    implicit class Creators(tp: (Double, Int)) {
      implicit def ~ : ProbaEvent = ProbaEventImpl.apply(tp._1, tp._2)

      implicit def ev(dCount: Double, dProba: Double): ProbaEvolution = new ProbaEvolutionImpl(tp._1 p, tp._2, dCount, dProba)
    }

    implicit class CreatorsFromOne(tp: Double) {
      implicit def p: Proba = ProbaImpl(tp)

    }

  }

  object ProbaEventImpl {
    def apply(proba: Double, periodCount: Double): ProbaEvent = {
      new ProbaEventImpl(ProbaImpl(proba), periodCount)
    }


  }

  case class ProbaEventImpl(proba: Proba,
                            periodNb: Double) extends ProbaEventBase[Proba] with ProbaEvent

  sealed trait ProbaEvent extends ProbaEventBase[Proba]

  sealed trait ProbaEventBase[T <: Proba] {
    def proba: Proba

    def periodNb: Double

    def draw(cntPerdiod: Int): Boolean = {
      if (cntPerdiod % periodNb.round == 0) {
        proba.draw
      } else {
        false
      }
    }

    def drawAndDo(cntPerdiod: Int)(f: () => Unit): Unit = {
      if (draw(cntPerdiod)) {
        f()
      }
    }
  }


}
