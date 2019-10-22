package bon

import slick.jdbc.JdbcProfile

trait H2  {val profile: JdbcProfile = slick.jdbc.H2Profile;val profileName: String = "h2mem1"}
