package channels.broadcast.authz

import channels.crypto.Commitment.RevealedValue
import channels.crypto.Hash
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object AuthzMessage {
  final case class DeltaValue(event: Hash, value: RevealedValue)

  private given bytesCodec: JsonValueCodec[Array[Byte]] = new JsonValueCodec[Array[Byte]] {
    def decodeValue(in: JsonReader, default: Array[Byte]): Array[Byte] = in.readBase64AsBytes(default)
    def encodeValue(x: Array[Byte], out: JsonWriter): Unit             = out.writeBase64Val(x, true)
    def nullValue: Array[Byte]                                         = Array.emptyByteArray
  }

  given codec: JsonValueCodec[AuthzMessage] = JsonCodecMaker.make
}

/** Messages of the [[AuthzProtocol]]. */
enum AuthzMessage {

  /** Signed events, encoded exactly as they are hashed, in an order in which they can be received. */
  case Events(events: List[Array[Byte]])

  /** Revealed values of delta commitments, filtered for what the receiver may read. */
  case Deltas(deltas: List[AuthzMessage.DeltaValue])

  /** Ask for the given events (and the deltas the requester may read). */
  case Request(events: Set[Hash])

  /** Ask for the deltas of the given (known) events, answered with those the requester may read. */
  case RequestDeltas(events: Set[Hash])

  /** Ask for everything, sent by replicas that do not even know the genesis event yet. */
  case Bootstrap
}
