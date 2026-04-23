package replication

import channels.{ArrayMessageBuffer, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import rdts.base.Historized.MetaDelta
import rdts.base.Uid
import rdts.time.Dots

object ProtocolMessage {

  /** `knows` has to be a subset of the dots known at the sender.
    * The sender of the request should then eventually receive all known missing dots.
    */
  case class Graft(sender: Uid, knows: Dots) extends ProtocolMessage[Nothing]

  /** Guarantees that for two payloads a and b, that if a.dots <= b.dots,
    * then a.data <= b.data according to the lattice of T
    */
  case class Payload[+T](dots: Dots, data: T, redundantDots: Dots)
      extends ProtocolMessage[T] {}
  object Payload                 {
    def apply[T](dots: Dots, data: T): Payload[T] =
      Payload(dots, data, Dots.empty)

    // this kinda makes sense, but kinda does not
    // given [T: Lattice]: Lattice[Payload[T]] = Lattice.derived

    extension [T](payloads: Iterable[Payload[T]]) {
      inline def getAllDots: Dots =
        payloads.foldLeft(Dots.empty)((dots, payload) => dots.union(payload.dots.union(payload.redundantDots)))

      inline def mapDeltas[A](f: T => A): Iterable[Payload[A]] =
        payloads.map(bufferedPayload => bufferedPayload.copy(data = f(bufferedPayload.data)))

      inline def toMetaDeltas: Iterable[MetaDelta[T]] =
        payloads.map(payload => MetaDelta(payload.dots, payload.data, payload.redundantDots))
    }
  }

  /** Lazy advertisement for Plumtree-style dissemination.
    * `knows` summarizes what the sender already has.
    */
  case class IHave(sender: Uid, knows: Dots) extends ProtocolMessage[Nothing]

  /** Request sender to move this edge to lazy mode for eager push.
    */
  case class Prune(sender: Uid) extends ProtocolMessage[Nothing]

  case class Ping(time: Long) extends ProtocolMessage[Nothing]
  case class Pong(time: Long) extends ProtocolMessage[Nothing]
}

sealed trait ProtocolMessage[+T]

trait CachedMessage[+T] {
  def messageBuffer: MessageBuffer
  def payload: T
}

class ReceivedCachedMessage[T: JsonValueCodec](val messageBuffer: MessageBuffer) extends CachedMessage[T] {
  val payload: T = readFromArray(messageBuffer.asArray)
}

class SentCachedMessage[T: JsonValueCodec](val payload: T) extends CachedMessage[T] {
  val messageBuffer: MessageBuffer = ArrayMessageBuffer(writeToArray(payload))
}
