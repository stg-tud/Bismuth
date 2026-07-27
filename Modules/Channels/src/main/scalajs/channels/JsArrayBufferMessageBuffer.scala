package channels

import channels.connection.MessageBuffer

import java.nio.ByteBuffer
import scala.scalajs.js.typedarray.TypedArrayBufferOps.bufferOps
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, TypedArrayBuffer, given}

class JsArrayBufferMessageBuffer(val inner: ArrayBuffer) extends MessageBuffer {
  override def convertToArray(): Array[Byte] = new Int8Array(inner).toArray
  override def asByteBuffer: ByteBuffer      = TypedArrayBuffer.wrap(inner)
}

object JsArrayBufferMessageBuffer {

  // TODO: likely unused, as all cases are handled by the case distinction in the extension methods
  //  particularly the else case is likely untested
  def convertByteBufferToArrayBuffer(buffer: ByteBuffer): ArrayBuffer = {
    if buffer.hasArrayBuffer() then {
      val offset = buffer.arrayBufferOffset() + buffer.position()
      val length = buffer.remaining()
      val ab     = buffer.arrayBuffer()
      if offset == 0 && length == ab.byteLength
      then ab
      else ab.slice(offset, offset + length)
    } else {
      val length = buffer.remaining()
      val copy   = ByteBuffer.allocateDirect(length).order(buffer.order())
      copy.put(buffer.duplicate())
      copy.flip()
      copy.arrayBuffer()
    }
  }

  extension (mb: MessageBuffer)
      def convertToArrayBuffer(): ArrayBuffer =
        mb match
            case buf: JsArrayBufferMessageBuffer => buf.inner
            case other                           =>
              convertByteBufferToArrayBuffer(mb.asByteBuffer)
}
