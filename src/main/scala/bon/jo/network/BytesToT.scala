package bon.jo.network

case class BytesToT[T](des: Array[Byte] => T)
