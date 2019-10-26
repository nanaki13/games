package bon.jo.network

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers, Unmarshal}
import akka.stream.ActorMaterializer
import bon.jo.DoIt
import bon.jo.FutureUtil._
import bon.jo.conf.{Conf, SerUNerOption, SerUnserUtil}
import bon.jo.controller.Scores
import bon.jo.model.Score

import scala.concurrent.Future

object MitronClient extends Log {

  implicit def unmarshallerScore: FromEntityUnmarshaller[Scores] = {
    PredefinedFromEntityUnmarshallers.byteArrayUnmarshaller.map(SerUnserUtil._readObject[Scores])

  }


  implicit val opt: SerUNerOption = Conf.outFile.copy(filePath = Conf.outFile.filePath)
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher


  def getMaxScores: Future[Scores] = {
    val req = HttpRequest(uri = Conf.url)
    Http().singleRequest(req).flatMap(Unmarshal(_).to[Scores])
  }

  def writeScore(test: Score): Future[String] = {
    val req = HttpRequest(
      method = HttpMethods.POST,
      uri = Conf.url,
      entity = HttpEntity(ContentTypes.`application/octet-stream`, SerUnserUtil.writeObject_(test))
    )
    Http().singleRequest(req).flatMap(Unmarshal(_).to[String])

  }
}
