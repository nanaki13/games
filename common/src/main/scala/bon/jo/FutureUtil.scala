package bon.jo

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

object FutureUtil {
  implicit val default =  Duration(20000, TimeUnit.MILLISECONDS)
}
