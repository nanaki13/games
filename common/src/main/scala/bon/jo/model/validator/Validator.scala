package bon.jo.model.validator

import bon.jo.model.Score

object Validator {

  def valid(score: Score)(implicit errorMessae : StringBuilder): Boolean = {
    val ret = score.who.map(_.length > 2).reduce(_ && _)
    if(!ret) errorMessae.append("Un nom plus grand faignant")
    ret
  }

}
