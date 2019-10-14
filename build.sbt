name := """mitron"""
lazy val commonSettings = Seq(
  organization := "bon.jo",
  version := "1.1",
  scalaVersion := "2.13.1"
)

//lazy val wr = (project in file("works-repository")).settings(
   // commonSettings,
    // other settings
 // )
lazy val root = (project in file(".")).settings(
  commonSettings,
  watchSources ++= (baseDirectory.value / "public/ui" ** "*").get
)
//.dependsOn(wr)
//.enablePlugins(PlayScala)

resolvers += Resolver.sonatypeRepo("snapshots")

//libraryDependencies += guice
//libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
//libraryDependencies += "com.h2database" % "h2" % "1.4.196"

