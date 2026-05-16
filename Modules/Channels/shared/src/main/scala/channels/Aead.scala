package channels

import scala.util.Try

trait Aead {
  def encrypt(plain: Array[Byte], associated: Array[Byte]): Array[Byte]
  def decrypt(cypher: Array[Byte], associated: Array[Byte]): Try[Array[Byte]]
}

object Aead {
  val emptyAssociatedData: Array[Byte] = Array.emptyByteArray

  val identity: Aead = new Aead {
    override def encrypt(plain: Array[Byte], associated: Array[Byte]): Array[Byte] = plain

    override def decrypt(cypher: Array[Byte], associated: Array[Byte]): Try[Array[Byte]] = Try(cypher)
  }
}
