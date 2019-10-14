package bon.jo.network

case class Message[T](length: Int, array: Array[Byte]) {
  def toObject(implicit des: Array[Byte] => T): Any = {
    val ret: Any = des(this.array)

  }

  def asUtf8: String = {
    new String(array, "utf-8")
  }

  def toType[T](implicit tr: BytesToT[T]): T = {
    val ret: T = tr.des(this.array)

    ret
  }
}

object Message {


  def apply[T](t: T)(implicit undes: T => Array[Byte]): Message[T] = {
    val by = undes(t)
    val lenght = by.length
    Message[T](lenght, by)
  }
}