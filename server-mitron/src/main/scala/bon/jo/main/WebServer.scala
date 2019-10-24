package bon.jo.main

import java.io.FileOutputStream
import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import akka.util.ByteString
import bon.jo.ScoreServiceImpl
import bon.jo.conf.{Conf, SerUNerOption}
import bon.jo.controller.Scores
import bon.jo.model.Score
import bon.{PostGres, ScoreRepo}
import scala.jdk.CollectionConverters._
import scala.concurrent.ExecutionContext
import scala.io.StdIn





class repoImpl extends PostGres with ScoreRepo   {
  override implicit def ec: ExecutionContext = scala.concurrent.ExecutionContext.global
}
case class service( repo: ScoreRepo) extends ScoreServiceImpl
object WebSeverUtil{
  implicit val templateParam = ScoresHtml.withBootestrap
  def entity(scores: Scores) : ResponseEntity =  HttpEntity(ScoresHtml.htmlTemplateSorted(scores))
  def entity(str: String) : ResponseEntity =  HttpEntity(ScoresHtml.html(str))
}
object WebServer {
  implicit val system = ActorSystem("my-system")

  implicit val executionContext = system.dispatcher

  implicit val opt: SerUNerOption = Conf.outFile.copy(filePath = Conf.outFile.filePath + "_server")
  implicit val materializer = ActorMaterializer()

  val dbProfile = () => new PostGres {}



  val repoSerive = new service(new repoImpl)
  def main(args: Array[String]) {
    import MessageSupport._


    val route: Route = concat(
      path("html"/ "xyzrtaez") {
        get {
          {
            extractRequest {
              req =>
                println(req.headers)
                req.entity match {
                  case _: HttpEntity.Strict =>

                    val htmlREsp =  HttpResponse(entity = HttpEntity(HtmlTemplateSys.template.toHTMLString).withContentType(ContentTypes.`text/html(UTF-8)`))
                    complete {
                      htmlREsp
                    }
                  case _ =>
                    complete("Ooops, request entity is not strict!")
                }

            }

            //   complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
          }
        }
      },
      path("html"/ "scores") {
        get {
          {
            extractRequest {
              req =>
                println(req.headers)
                req.entity match {
                  case _: HttpEntity.Strict =>
                    val resp = repoSerive.allInScores

                    val htmlREsp = resp.map( e  => HttpResponse(entity = WebSeverUtil.entity(e).withContentType(ContentTypes.`text/html(UTF-8)`)))
                    complete {
                      htmlREsp
                    }
                  case _ =>
                    complete("Ooops, request entity is not strict!")
                }

            }

            //   complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
          }
        }
      },
      path("scores") {
        get {
          {
            extractRequest {
              req =>
                println(req.headers)
                req.entity match {
                  case _: HttpEntity.Strict =>
                    val resp = repoSerive.allInScores
                    complete {
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
            entity(as[Score]) { s =>
              complete {
                println("Score received")
               val saved =     repoSerive.save(s)
                println(s"res from db : $saved")
                "Ok"
                // ... write order to DB
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

    val (host,port )= if (args.length > 0){
      ("0.0.0.0",args(0).toInt)
    }else{
      ("localhost",8080)
    }
    println(host,port )
    Http().bindAndHandle(route, host, port)

    println(s"Server online at http://$host:${port}/")

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



