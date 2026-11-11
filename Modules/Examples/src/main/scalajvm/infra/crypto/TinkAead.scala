package infra.crypto

import channels.connection.MessageBuffer
import channels.experiments

import java.nio.ByteBuffer
import scala.util.Try

/** Adapts a Tink [[com.google.crypto.tink.Aead]] to the channels [[experiments.Aead]] interface. */
class TinkAead(aead: com.google.crypto.tink.Aead) extends experiments.Aead {
  override def encrypt(data: ByteBuffer, associated: ByteBuffer): ByteBuffer =
    ByteBuffer.wrap(aead.encrypt(
      MessageBuffer.convertByteBufferToArray(data),
      MessageBuffer.convertByteBufferToArray(associated)
    ))

  override def decrypt(data: ByteBuffer, associated: ByteBuffer): Try[ByteBuffer] =
    Try(ByteBuffer.wrap(aead.decrypt(
      MessageBuffer.convertByteBufferToArray(data),
      MessageBuffer.convertByteBufferToArray(associated)
    )))
}
