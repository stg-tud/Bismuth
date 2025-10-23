package network

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import java.security.PublicKey

case class Message(
                    tag: Tag,
                    sender: Array[Byte],
                    payload: Array[Byte]
                  )

object Message:
  given JsonValueCodec[Message] = JsonCodecMaker.make

enum Tag:
  case SEND_CODED_SYMBOLS, CODED_SYMBOLS, DELTA, REQUEST_DELTA, SYNC_DONE
