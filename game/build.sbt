
import java.nio.file.Paths

import NativePackagerHelper._
enablePlugins(JavaAppPackaging)

mainClass in Compile :=  Some("bon.jo.main.MainMitron")
mainClass in assembly :=  Some("bon.jo.main.MainMitron")
mappings in Universal ++= {

  val jresDir = Paths.get( "I:/work/jdk/")
  val windowsJreDir = jresDir.resolve("jdk-11.0.4+11-jre")
  directory(windowsJreDir.toFile).map { j =>
    j._1 -> j._2.replace("jdk-11.0.4+11-jre", "jre")
  }
}

javaOptions in Universal ++= Seq(
  // Your original options

  "-java-home ${app_home}/../jre"
)