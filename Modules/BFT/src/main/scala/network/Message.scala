package network

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

case class Message(
    tag: Tag,
    sender: String,
    payload: Array[Byte]
)

object Message:
   given JsonValueCodec[Message] = JsonCodecMaker.make

enum Tag:
   case CODED_SYMBOLS_REQUEST, CODED_SYMBOLS, DELTA, REQUEST_DELTA
