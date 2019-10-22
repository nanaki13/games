package bon

import slick.jdbc.JdbcProfile

trait WithProfile {
  val profile: JdbcProfile

  def profileName: String
}
