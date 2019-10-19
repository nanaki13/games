
import sbt.Keys.{libraryDependencies, mainClass}

name := """mitron"""
val _scalaVersion = "2.13.1"
lazy val commonSettings = Seq(
  organization := "bon.jo",
  version := "1.1",
  scalaVersion := _scalaVersion
)


lazy val test = Seq( "org.scalatest" %% "scalatest" % "3.0.8")
lazy val repoDep = Seq(   "com.typesafe.slick" %% "slick" % "3.3.2",
  "com.h2database" % "h2" % "1.4.192"
  ,"org.postgresql" % "postgresql" %"42.2.5")
//lazy val global = project
//  .in(file("glob"))
//  .settings(commonSettings)
//  .aggregate(
//  common
//  )


resolvers += Resolver.sonatypeRepo("snapshots")


lazy val common = project.settings(
  commonSettings,

)
lazy val `score-repo` = project.settings(
  commonSettings,
  libraryDependencies ++= (test.map(_ % Test) ++ repoDep)


).dependsOn(common)
lazy val `server-mitron` = project.settings(
  commonSettings,
  mainClass in Compile := Some("bon.jo.main.WebServer"),
  libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-http"   % "10.1.10","com.typesafe.akka" %% "akka-stream" % "2.5.23" )

).dependsOn(common,`score-repo`)
lazy val game = project.settings(
  commonSettings,
  libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-http"   % "10.1.10","com.typesafe.akka" %% "akka-stream" % "2.5.23" )
).dependsOn(common)


lazy val root = (project in file("."))
  .settings(
    commonSettings,
    mainClass in Compile := Some("bon.jo.main.WebServer")
  )
  .aggregate(
    common, `score-repo`,`server-mitron`
  ).dependsOn(common,`score-repo`,`server-mitron`)
enablePlugins(JavaAppPackaging)
//.enablePlugins(PlayScala)


// or whatever the latest version is
//libraryDependencies += guice
//libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
//libraryDependencies += "com.h2database" % "h2" % "1.4.196"

