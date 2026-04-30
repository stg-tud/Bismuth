package channels

import java.net.ServerSocket
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.regex.Pattern
import java.util.{Base64, Scanner};

sealed trait WebsocketFrame

sealed trait DataFrame                    extends WebsocketFrame
case class TextFrame(text: String)        extends DataFrame
case class BinaryFrame(data: Array[Byte]) extends DataFrame

sealed trait ControlFrame                                extends WebsocketFrame
case class CloseFrame(code: Option[Int], reason: String) extends ControlFrame
case class PingFrame(data: Array[Byte])                  extends ControlFrame
case class PongFrame(data: Array[Byte])                  extends ControlFrame

// Internal/edge cases that preserve raw details
case class ReservedFrame(opcode: Int, data: Array[Byte]) extends WebsocketFrame

object SimpleWebsocket {
  def main(args: Array[String]): Unit = {
    val server = new ServerSocket(8080);
    System.out.println("Server has started on :80.\r\nWaiting for a connection…");
    val client = server.accept();
    System.out.println("A client connected.")

    val in  = client.getInputStream
    val out = client.getOutputStream
    val s   = new Scanner(in, StandardCharsets.UTF_8)

    val data = s.useDelimiter("\\r\\n\\r\\n").next
    val get  = Pattern.compile("^GET").matcher(data)

    if get.find() then {
      val `match` = Pattern.compile("Sec-WebSocket-Key: (.*)").matcher(data)
      `match`.find()
      println(`match`.group(1))
      val response =
        ("HTTP/1.1 101 Switching Protocols\r\n" +
          "Connection: Upgrade\r\n" +
          "Upgrade: websocket\r\n" +
          "Sec-WebSocket-Accept: " +
          Base64.getEncoder.encodeToString(
            MessageDigest.getInstance("SHA-1").digest(
              (`match`.group(1) + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").getBytes("UTF-8")
            )
          ) + "\r\n\r\n").getBytes(StandardCharsets.UTF_8)
      out.write(response, 0, response.length)
    }

    def readByte() =
        val res = in.read()
        if res < 0 then throw IllegalStateException("socket closed")
        res.toByte

    // Fragment buffer: list of payload chunks and the message type (1=text, 2=binary)
    var fragments: List[Array[Byte]] = Nil
    var fragmentType: Option[Int]    = None // 0x1 = text, 0x2 = binary

    def readFrame(): Option[WebsocketFrame] =
        val firstByte = readByte()
        val fin       = (firstByte & 128) != 0
        val opcode    = firstByte & 15

        val secondByte = readByte()
        val mask       = (secondByte & 128) != 0
        val len        = secondByte & 127

        val length: Long = len match
            case 126 =>
              val upper = readByte()
              val lower = readByte()
              (upper.toInt << 16) | (lower.toInt)
            case 127 =>
              ByteBuffer.wrap(in.readNBytes(8)).getLong
            case other => other

        val maskKey =
          if mask
          then in.readNBytes(4)
          else Array[Byte](0, 0, 0, 0)

        if length > Int.MaxValue then throw IllegalStateException("message too large")

        val payload =
            val encoded                = in.readNBytes(length.toInt)
            def rec(offset: Int): Unit =
                if offset >= encoded.length then return
                encoded.update(offset, (encoded(offset) ^ maskKey(offset & 3)).toByte)
                rec(offset + 1)
            rec(0)
            encoded

        opcode match
            // Control frames - handle immediately, never fragmented
            case 0x8 =>
              val code   = if payload.length >= 2 then Some((payload(0) & 0xff) << 8 | (payload(1) & 0xff)) else None
              val reason =
                if payload.length > 2 then new String(payload, 2, payload.length - 2, StandardCharsets.UTF_8) else ""
              Some(CloseFrame(code, reason))

            case 0x9 => Some(PingFrame(payload))

            case 0xa => Some(PongFrame(payload))

            // Data frames - buffer and assemble
            case 0x1 | 0x2 | 0x0 =>
              // Set type for text/binary, keep existing type for continuation
              if opcode != 0 then fragmentType = Some(opcode)
              fragments = payload :: fragments

              if fin then
                  // Assemble complete message
                  val completeData = fragments.reverse.flatten.toArray
                  val msgType      = fragmentType.getOrElse(throw IllegalStateException("FIN without message type"))

                  val frame: WebsocketFrame = msgType match
                      case 0x1 => TextFrame(new String(completeData, StandardCharsets.UTF_8))
                      case 0x2 => BinaryFrame(completeData)

                  // Reset buffer
                  fragments = Nil
                  fragmentType = None

                  Some(frame)
              else None

            // Reserved opcodes
            case other =>
              Some(ReservedFrame(other, payload))

    while
        val frame = readFrame()
        !frame.exists(_.isInstanceOf[CloseFrame])
    do ()

  }
}
