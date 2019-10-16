package bon.jo.main

import java.io.FileOutputStream
import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.MediaTypes.`application/octet-stream`
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers}
import akka.stream.ActorMaterializer
import akka.util.ByteString
import bon.jo.conf.{Conf, SerUNerOption, SerUnserUtil}
import bon.jo.model.Scores

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.{Failure, Success}


//object ZeroFormatterSupport extends MessageSupport {
//  override def serialize(value: Message[Scores]): Array[Byte] = SerUnserUtil.writeObject_(value.value)
//
//  override def deserialize(bytes: Array[Byte]) = Message.apply(bytes.length, bytes)
//}

object MessageSupport {

  //  def serialize(value: Message[Scores]): Array[Byte]
  //
  //  def deserialize(bytes: Array[Byte]): Message[Scores]

  //  private val zeroFormatterUnmarshaller =
  //    Unmarshaller.byteFromStringUnmarshaller(
  //  .forContentTypes(`application/octet-stream`)
  //      .map {
  //        case Array() => throw Unmarshaller.NoContentException
  //        case data => data
  //      }

  private val zeroFormatterMarshaller = Marshaller.byteArrayMarshaller(`application/octet-stream`)


  //  implicit def unmarshaller: FromEntityUnmarshaller[Message[Scores]] = {
  //    zeroFormatterUnmarshaller
  //      .map(bs => deserialize(bs))
  //  }

  implicit def unmarshallerScore[T <: Product]: FromEntityUnmarshaller[T] = {
    PredefinedFromEntityUnmarshallers.byteArrayUnmarshaller.map(SerUnserUtil._readObject[T])

  }

  implicit def marshalle[T <: Product]: ToEntityMarshaller[T] =
    zeroFormatterMarshaller
      .compose(SerUnserUtil.writeObject_)
}

object WebServer {
  implicit val system = ActorSystem("my-system")

  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher

  implicit val opt: SerUNerOption = Conf.outFile.copy(filePath = Conf.outFile.filePath + "_server")
  implicit val materializer = ActorMaterializer()

  def main(args: Array[String]) {
    import MessageSupport._


    val route: Route = concat(
      path("max") {
        get {
          {
            extractRequest {
              req =>
                println(req.headers)
                req.entity match {
                  case _: HttpEntity.Strict =>
                    val resp = SerUnserUtil.readObject(Scores.empty)
                    complete {
                      println(resp)
                      resp
                    }
                  case _ =>
                    complete("Ooops, request entity is not strict!")
                }

            }

            //   complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
          }
        }
      }, path("scores") {
        post {
          // decompress gzipped or deflated requests if required
          decodeRequest {
            // unmarshal with in-scope unmarshaller
            entity(as[Scores]) { order =>
              complete {
                // ... write order to DB
                println("Order received")
                order
              }
            }
          }
        }

      },
      path("scores_") {
        (post & entity(as[Multipart.FormData])) { fileData =>
          complete {
            val fileName = UUID.randomUUID().toString
            val temp = System.getProperty("java.io.tmpdir")
            val filePath = "./" + fileName
            processFile(filePath, fileData).map { fileSize =>
              HttpResponse(StatusCodes.OK, entity = s"File successfully uploaded. Fil size is $fileSize")
            }.recover {
              case ex: Exception => HttpResponse(StatusCodes.InternalServerError, entity = "Error in file uploading")
            }
          }
        }
      }, path("form.html") {
        get {
          complete {
            HttpResponse(entity = HttpEntity(
              ContentTypes.`text/html(UTF-8)`,
              s"""
              <!DOCTYPE html>
                 |<html>
                 |<body>
                 |
 |<form action="scores_" method="post" enctype="multipart/form-data">
                 |    Select image to upload:
                 |    <input type="file" name="data" id="data">
                 |    <input type="submit" value="Upload Image" name="submit">
                 |</form>
                 |
 |</body>
                 |</html>
           """.stripMargin))
          }
        }
      })

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }

  private def processFile(filePath: String, fileData: Multipart.FormData) = {
    val fileOutput = new FileOutputStream(filePath)
    fileData.parts.mapAsync(1) { bodyPart ⇒
      def writeFileOnLocal(array: Array[Byte], byteString: ByteString): Array[Byte] = {
        val byteArray: Array[Byte] = byteString.toArray
        fileOutput.write(byteArray)
        array ++ byteArray
      }

      bodyPart.entity.dataBytes.runFold(Array[Byte]())(writeFileOnLocal)
    }.runFold(0)(_ + _.length)
  }
}

object TestClient extends App{

  implicit val opt: SerUNerOption = Conf.outFile.copy(filePath = Conf.outFile.filePath)
  val scores =  SerUnserUtil.readObject[Scores](null)

  val req = HttpRequest(
    method = HttpMethods.POST,
    uri = "http://localhost:8080/scores",
    entity = HttpEntity(ContentTypes.`application/octet-stream`,  SerUnserUtil.writeObject_(scores))
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