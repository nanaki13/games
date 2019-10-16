package bon.jo.network


import bon.jo.model.{Score, Scores}

trait MitronClient extends ClientServeur {
  implicit val cl = classOf[Scores]

  import ClientServeur._

  def exit = write(Message("exit")) match {
    case 'A' => "OK"
    case _ => "KO"
  }

  def getMaxScores: Scores = {
    write(Message("maxScore")) match {
      case 'A' => read[Scores].toType
    }
  }

  def writeScore(s: Score) = {
    log("send write score message")
    write(Message("writeScore")(ClientServeur.ser)) match {
      case 'A' => {
        log("serveur say ok")
        log("send the  message")
        val m = Message[Score](s)
        write(m) match {
          case 'A' => {
            log("server say ok for write score");
            "OK"
          }
          case _ => {
            log("server say other for write score");
            "OK"
          }
        }
      }
      case _ => "KO"
    }
  }
}
