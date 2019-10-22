package bon.jo

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object DoIt{
  def now[T](future: Future[T])(implicit duration: Duration):T = {
    Await.result(future,duration)
  }
}
