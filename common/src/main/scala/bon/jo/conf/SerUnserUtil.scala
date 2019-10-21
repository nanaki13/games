package bon.jo.conf

import java.io._

object SerUnserUtil {
  def writeObject(scores: Product)(implicit fileName: SerUNerOption) = {
    val fw = new ObjectOutputStream(new FileOutputStream(fileName.filePath))
    fw.writeObject(scores)
    fw.close()
  }

  def readObject[T](none: T)(implicit fileName: SerUNerOption): T = {
    if ((new File(fileName.filePath)).exists()) {
      val fw = new ObjectInputStream(new FileInputStream(fileName.filePath))
      try{
        val ret = fw.readObject().asInstanceOf[T]

        ret
      }catch {
        case e : Exception => e.printStackTrace();fw.close();none
      } finally fw.close()

    } else {
      none
    }
  }

  def writeObject_(scores: Product): Array[Byte] = {
    val o = new ByteArrayOutputStream
    val fw = new ObjectOutputStream(o)
    fw.writeObject(scores)
    fw.flush()
    val ret = o.toByteArray
    fw.close()
    ret
  }

  def _readObject[T](obj: Array[Byte]): T = {

    val fw = new ObjectInputStream(new ByteArrayInputStream(obj))
    try{
      fw.readObject().asInstanceOf[T]

    }catch {
      case e : Exception =>  e.printStackTrace();fw.close();null.asInstanceOf[T]
    } finally fw.close()


  }
}
