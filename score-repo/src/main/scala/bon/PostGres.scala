package bon

import slick.jdbc.JdbcProfile

trait PostGres  {val profile: JdbcProfile = slick.jdbc.PostgresProfile;val profileName: String = "postgres"}


