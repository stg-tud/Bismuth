package channels

import channels.connection.MessageBuffer

import java.nio.ByteBuffer
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, TypedArrayBuffer, given}

class JsArrayBufferMessageBuffer(val inner: ArrayBuffer) extends MessageBuffer {
  override def convertToArray(): Array[Byte] = new Int8Array(inner).toArray
  override def asByteBuffer: ByteBuffer      = TypedArrayBuffer.wrap(inner)
}

object MesageBufferExtensions {
  extension (mb: MessageBuffer)
      def asArrayBuffer: ArrayBuffer =
        mb match
            case buf: JsArrayBufferMessageBuffer => buf.inner
            case other                           => other.convertToArray().toTypedArray.buffer
}
