package bon.jo.main

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.MediaTypes.`application/octet-stream`
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers}
import bon.jo.conf.SerUnserUtil
import bon.jo.controller.Scores
import bon.jo.model.Score

object MessageSupport {



  private val zeroFormatterMarshaller = Marshaller.byteArrayMarshaller(`application/octet-stream`)

  implicit def unmarshallerScores: FromEntityUnmarshaller[Scores] = {
    PredefinedFromEntityUnmarshallers.byteArrayUnmarshaller.map(SerUnserUtil._readObject[Scores])

  }

  implicit def marshalleScores: ToEntityMarshaller[Scores] =
    zeroFormatterMarshaller
      .compose(SerUnserUtil.writeObject_)


  implicit def unmarshallerScore: FromEntityUnmarshaller[Score] = {
    PredefinedFromEntityUnmarshallers.byteArrayUnmarshaller.map(SerUnserUtil._readObject[Score])

  }

  implicit def marshalleScore: ToEntityMarshaller[Score] =
    zeroFormatterMarshaller
      .compose(SerUnserUtil.writeObject_)
}
