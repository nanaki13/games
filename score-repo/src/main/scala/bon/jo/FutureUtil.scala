package bon.jo

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object FutureUtil {
  implicit class FutureResolve[T](val f: Future[T]) {
    def resolve: T = {
      Await.result(f, Duration.Inf)
    }
    def unary_~ : T = {
      resolve
    }
  }
}
