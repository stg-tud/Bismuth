package channels.experiments

import java.nio.ByteBuffer
import scala.util.Try

trait Aead {
  def encrypt(plain: ByteBuffer, associated: ByteBuffer): ByteBuffer
  def decrypt(cypher: ByteBuffer, associated: ByteBuffer): Try[ByteBuffer]
}

object Aead {
  val emptyAssociatedData: ByteBuffer = ByteBuffer.allocate(0)

  val identity: Aead = new Aead {
    override def encrypt(plain: ByteBuffer, associated: ByteBuffer): ByteBuffer = plain

    override def decrypt(cypher: ByteBuffer, associated: ByteBuffer): Try[ByteBuffer] = Try(cypher)
  }
}
