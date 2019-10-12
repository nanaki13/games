package bon.jo.network

import java.io.{DataInputStream, DataOutputStream}
import java.net.{ServerSocket, Socket}

import bon.jo.conf.Conf
import bon.jo.controller.{Score, Scores, SerUNerOption, SerUnserUtil}

import scala.concurrent.Future

object MitronServeur extends App with Log {
  import ClientServeur._

  import scala.concurrent.ExecutionContext.Implicits.global

println(s"start on : ${Conf.serverPort}")
  val sc = new ServerSocket(Conf.serverPort)
  implicit val cl = classOf[Scores]

  override def name = "serveur_" + Thread.currentThread()

  class Connection(val clientSocket: Socket) extends ClientServeur {
    val in = new DataInputStream(clientSocket.getInputStream)
    val out = new DataOutputStream(clientSocket.getOutputStream)

    override def name = "serveur_client_" + Thread.currentThread()
  }

  while (true) {
    log("waiting for client")
    try {
      val connection: Connection = new Connection(sc.accept())
      Future {
        while (!connection.clientSocket.isClosed) {
          log("client come")
          var opCnt: Int = 0
          // var scoresOption  :Option[Scores] = None
          //     implicit val unser_ : BytesToT[_] = unser

          implicit val opt: SerUNerOption = Conf.outFile.copy(filePath = Conf.outFile.filePath + "_server")

          def scores = SerUnserUtil.readObject(Scores.empty)

          val resultProcessClient: Any = connection.read[String].toType[String] match {
            case "maxScore" => {
              val scoresOption = scores
              connection.write(Message(scoresOption)) match {
                case 'A' => "OK"
              }

            }
            case "writeScore" => {
              log(s"wait read score")
              implicit val cl: Class[Score] = classOf[Score]

              val sc: Score = connection.read[Score].toType
              implicit val nbJ: Int =  sc.nbJ
              implicit val game: String =  sc.game
              val scoresOption = scores

              scoresOption addIfBest sc
              log(s"we have receive this new score : $sc")
              SerUnserUtil.writeObject(scoresOption)
              "OK"
            }
            case "exit" => {
              log(s"exit"); connection.clientSocket.close(); "OK"
            }
            case _ => {
              log(s"unknow ùessage : close"); connection.clientSocket.close(); "OK"
            }
          }
          opCnt += 1
          log(s"client proces result ${opCnt}  : $resultProcessClient")


        }
      }

    } catch {
      case exception: Exception => exception.printStackTrace()
    }


  }
  sc.close()


}
