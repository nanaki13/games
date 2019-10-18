package bon.jo.test

import bon.{H2, ScoreRepo}

trait TestRepoImpl extends H2 with ScoreRepo
