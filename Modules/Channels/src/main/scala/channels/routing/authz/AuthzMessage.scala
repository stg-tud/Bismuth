package channels.routing.authz

import channels.broadcast.PlumtreeMessage
import channels.crypto.Hash
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object AuthzMessage {
  private given bytesCodec: JsonValueCodec[Array[Byte]] = new JsonValueCodec[Array[Byte]] {
    def decodeValue(in: JsonReader, default: Array[Byte]): Array[Byte] = in.readBase64AsBytes(default)
    def encodeValue(x: Array[Byte], out: JsonWriter): Unit             = out.writeBase64Val(x, true)
    def nullValue: Array[Byte]                                         = Array.emptyByteArray
  }

  given codec: JsonValueCodec[AuthzMessage] = JsonCodecMaker.make
}

/** Messages of the [[AuthzProtocol]]. */
enum AuthzMessage {

  /** Plumtree dissemination of the events, signed and encoded exactly as they are hashed. */
  case Events(message: PlumtreeMessage[Set[Hash], Array[Byte]])

  /** Revealed values of delta commitments, filtered for what the receiver may read. */
  case Deltas(deltas: List[DeltaValue])

  /** Ask for the deltas of the given (known) events, answered with those the requester may read. */
  case RequestDeltas(events: Set[Hash])
}
