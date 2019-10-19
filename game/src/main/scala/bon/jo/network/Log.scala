package bon.jo.network

import java.time.Instant

trait Log {
  def log(text: String) = println(s"${Instant.now()} : ${loggerName} : $text")

  val loggerName: String = this.getClass.getName
}
