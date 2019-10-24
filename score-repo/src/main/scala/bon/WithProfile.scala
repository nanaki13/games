package bon

import slick.jdbc.JdbcProfile

trait WithProfile {
  val profile: JdbcProfile

  def profileName: String
  def urlDb: Option[String] = Option(System.getenv("JDBC_DATABASE_URL")).map(_.replaceAll("\\?.*",""))
  def userDb: Option[String] = Option(System.getenv("JDBC_DATABASE_USERNAME"))
  def passwordDb: Option[String] = Option(System.getenv("JDBC_DATABASE_PASSWORD"))
}
