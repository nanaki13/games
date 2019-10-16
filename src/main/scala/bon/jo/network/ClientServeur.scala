package bon.jo.network

import java.io._

object ClientServeur {
  implicit def ser[T <: Serializable](t: T): Array[Byte] = {
    val _out = new ByteArrayOutputStream()
    val __out = new ObjectOutputStream(_out)
    __out.writeObject(t)
    __out.flush()
    __out.close()
    _out.toByteArray
  }

  implicit def unser[T]: BytesToT[T] = BytesToT((array: Array[Byte]) => {
    val fw = new ObjectInputStream(new ByteArrayInputStream(array))
    val ret = fw.readObject().asInstanceOf[T]
    fw.close()
    ret
  }
  )
}

trait ClientServeur extends Log {


  def out: DataOutputStream

  def in: DataInputStream


  def write(message: Message[_]) = {
    log(s" write length: ${message.length}")
    out.writeInt(message.length)
    log(s" write array")
    out.write(message._array)
    log(s" wait ok ")
    in.readChar()
  }

  def read[T]: Message[T] = {
    log(s" read message length ")
    val le = in.readInt()
    val by = new Array[Byte](le)
    log(s" read byte of message")
    in.read(by, 0, le)
    log(s" write ok")
    out.writeChar('A')
    Message[T](le, by)
  }
}