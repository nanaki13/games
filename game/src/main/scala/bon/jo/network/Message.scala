package bon.jo.network

import bon.jo.conf.SerUnserUtil

case class Message[T](length: Int = 0 , _array: Array[Byte] = null , vall : T = null.asInstanceOf[T]) {

  var value : T = vall

  def toObject(implicit des: Array[Byte] => T): Any = {
    val ret: Any = des(this._array)

  }

  def asUtf8: String = {
    new String(_array, "utf-8")
  }

  def toType(implicit tr: BytesToT[T]): T = {
    value = tr.des(this._array)
    value
  }
}

object Message {


  def apply[T](t: T)(implicit undes: T => Array[Byte]): Message[T] = {
    val undess = undes(t)
    Message[T](undess.size, undess,t)
  }
}