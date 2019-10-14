package bon.jo.controller

import java.io._

object SerUnserUtil {
  def writeObject(scores: Product)(implicit fileName: SerUNerOption) = {
    val fw = new ObjectOutputStream(new FileOutputStream(fileName.filePath))
    fw.writeObject(scores)
    fw.close()
  }

  def readObject[T]( none: T)(implicit fileName: SerUNerOption): T = {
    if ((new File(fileName.filePath)).exists()) {
      val fw = new ObjectInputStream(new FileInputStream(fileName.filePath))
      val ret = fw.readObject().asInstanceOf[T]
      fw.close()
      ret
    } else {
      none
    }
  }
}
