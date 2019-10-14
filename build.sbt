name := """mitron"""
val _scalaVersion = "2.13.1"
lazy val commonSettings = Seq(
  organization := "bon.jo",
  version := "1.1",
  scalaVersion := _scalaVersion
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
enablePlugins(JavaAppPackaging)
mainClass in Compile := Some("bon.jo.network.MitronServeur")
resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "org.scala-lang" % "scala-compiler" % _scalaVersion
//libraryDependencies += guice
//libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
//libraryDependencies += "com.h2database" % "h2" % "1.4.196"

