package bon.jo.network

import java.time.Instant

trait Log {
  def log(text: String) = println(s"${Instant.now()} : ${name} : $text")

  def name: String
}
