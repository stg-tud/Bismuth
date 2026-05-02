package replication

import channels.{ArrayMessageBuffer, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import rdts.base.Historized.MetaDelta
import rdts.base.Uid
import rdts.time.Dots


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
