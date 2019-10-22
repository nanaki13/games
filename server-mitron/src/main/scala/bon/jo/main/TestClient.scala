package bon.jo.main

import java.time.LocalDate

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer

import bon.jo.conf.{Conf, SerUNerOption, SerUnserUtil}
import bon.jo.model.Score

import scala.concurrent.Future
import scala.util.{Failure, Success}

object TestClient extends App{

  implicit val opt: SerUNerOption = Conf.outFile.copy(filePath = Conf.outFile.filePath)

  val test =  Score("mitron",LocalDate.now(),10,List("bob"))
  val req = HttpRequest(
    method = HttpMethods.POST,
    uri = "http://localhost:8080/scores",
    entity = HttpEntity(ContentTypes.`application/octet-stream`,  SerUnserUtil.writeObject_(test))
  )
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher

  val responseFuture: Future[HttpResponse] = Http().singleRequest(req)

  responseFuture
    .onComplete {
      case Success(res) => println(res)
      case Failure(_)   => sys.error("something wrong")
    }
}
