package bon.jo.test

import bon.{ PostGres, ScoreRepo}

trait TestRepoImpl extends PostGres with ScoreRepo
