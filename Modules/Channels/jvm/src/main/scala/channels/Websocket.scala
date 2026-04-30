package channels

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64

sealed trait WebsocketFrame

sealed trait DataFrame                    extends WebsocketFrame
case class TextFrame(text: String)        extends DataFrame
case class BinaryFrame(data: Array[Byte]) extends DataFrame

sealed trait ControlFrame                                extends WebsocketFrame
case class CloseFrame(code: Option[Int], reason: String) extends ControlFrame
case class PingFrame(data: Array[Byte])                  extends ControlFrame
case class PongFrame(data: Array[Byte])                  extends ControlFrame

case class ReservedFrame(opcode: Int, data: Array[Byte]) extends WebsocketFrame

object WebsocketProtocol {
  private val handshakePrefix = "GET ".getBytes(StandardCharsets.US_ASCII)
  private val headerEnd       = "\r\n\r\n".getBytes(StandardCharsets.US_ASCII)
  private val websocketGuid   = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

  final case class HandshakeRequest(path: String, headers: Map[String, String]) {
    def header(name: String): Option[String] = headers.get(name.toLowerCase)
  }

  final case class ParsedFrame(frame: WebsocketFrame, consumed: Int)

  def looksLikeHandshake(prefix: Array[Byte]): Boolean =
    prefix.length >= handshakePrefix.length && handshakePrefix.indices.forall(i => prefix(i) == handshakePrefix(i))

  def tryParseHandshake(buffer: Array[Byte]): Option[(HandshakeRequest, Int)] = {
    val end = indexOf(buffer, headerEnd)
    if end < 0 then None
    else {
      val headerBytes = java.util.Arrays.copyOfRange(buffer, 0, end)
      val headerText  = new String(headerBytes, StandardCharsets.US_ASCII)
      val lines       = headerText.split("\\r\\n").toList
      lines match {
        case requestLine :: rest if requestLine.startsWith("GET ") =>
          val path    = requestLine.drop(4).takeWhile(_ != ' ')
          val headers = rest.collect {
            case line if line.contains(":") =>
              val idx = line.indexOf(':')
              line.substring(0, idx).trim.toLowerCase -> line.substring(idx + 1).trim
          }.toMap
          Some(HandshakeRequest(path, headers) -> (end + headerEnd.length))
        case _ => None
      }
    }
  }

  def handshakeResponse(request: HandshakeRequest): Array[Byte] = {
    val key = request.header("sec-websocket-key").getOrElse {
      throw IllegalArgumentException("missing Sec-WebSocket-Key header")
    }

    val accept = Base64.getEncoder.encodeToString(
      MessageDigest.getInstance("SHA-1").digest((key + websocketGuid).getBytes(StandardCharsets.UTF_8))
    )

    val response =
      s"HTTP/1.1 101 Switching Protocols\r\n" +
      s"Connection: Upgrade\r\n" +
      s"Upgrade: websocket\r\n" +
      s"Sec-WebSocket-Accept: $accept\r\n" +
      s"\r\n"

    response.getBytes(StandardCharsets.US_ASCII)
  }

  def tryParseFrame(buffer: Array[Byte]): Option[ParsedFrame] = {
    if buffer.length < 2 then None
    else {
      val firstByte  = buffer(0) & 0xff
      val secondByte = buffer(1) & 0xff

      val fin    = (firstByte & 0x80) != 0
      val opcode = firstByte & 0x0f
      val masked = (secondByte & 0x80) != 0
      val lenTag = secondByte & 0x7f

      var offset              = 2
      val payloadLength: Long = lenTag match {
        case 126 =>
          if buffer.length < offset + 2 then return None
          val len = ((buffer(offset) & 0xff) << 8) | (buffer(offset + 1) & 0xff)
          offset += 2
          len.toLong
        case 127 =>
          if buffer.length < offset + 8 then return None
          val len = ByteBuffer.wrap(buffer, offset, 8).getLong
          offset += 8
          len
        case other => other.toLong
      }

      if payloadLength < 0 || payloadLength > Int.MaxValue then
          throw IllegalStateException(s"unsupported websocket payload length: $payloadLength")

      val maskKeyLength = if masked then 4 else 0
      if buffer.length < offset + maskKeyLength then return None

      val maskKey =
        if masked then java.util.Arrays.copyOfRange(buffer, offset, offset + 4)
        else Array[Byte](0, 0, 0, 0)
      offset += maskKeyLength

      val frameLength = offset + payloadLength.toInt
      if buffer.length < frameLength then return None

      val payload = java.util.Arrays.copyOfRange(buffer, offset, frameLength)
      if masked then {
        var i = 0
        while i < payload.length do
            payload(i) = (payload(i) ^ maskKey(i & 3)).toByte
            i += 1
      }

      val frame: WebsocketFrame = opcode match {
        case 0x1 => TextFrame(new String(payload, StandardCharsets.UTF_8))
        case 0x2 => BinaryFrame(payload)
        case 0x8 =>
          val code   = if payload.length >= 2 then Some(((payload(0) & 0xff) << 8) | (payload(1) & 0xff)) else None
          val reason =
            if payload.length > 2 then new String(payload, 2, payload.length - 2, StandardCharsets.UTF_8) else ""
          CloseFrame(code, reason)
        case 0x9 => PingFrame(payload)
        case 0xa => PongFrame(payload)
        case other if other == 0x0 || other == 0x3 || other == 0x4 || other == 0x5 || other == 0x6 || other == 0x7 =>
          ReservedFrame(other, payload)
        case other => ReservedFrame(other, payload)
      }

      Some(ParsedFrame(frame, frameLength | (if fin then 0x40000000 else 0)))
    }
  }

  def isFinalFrame(consumed: Int): Boolean = (consumed & 0x40000000) != 0
  def consumedBytes(consumed: Int): Int    = consumed & 0x3fffffff

  def encodeBinaryFrame(payload: Array[Byte]): Array[Byte] =
    encodeFrame(opcode = 0x2, payload = payload)

  def encodePongFrame(payload: Array[Byte]): Array[Byte] =
    encodeFrame(opcode = 0xa, payload = payload)

  def encodeCloseFrame(code: Int = 1000, reason: String = ""): Array[Byte] = {
    val reasonBytes = reason.getBytes(StandardCharsets.UTF_8)
    val payload     = ByteBuffer.allocate(2 + reasonBytes.length)
    payload.putShort(code.toShort)
    payload.put(reasonBytes)
    encodeFrame(opcode = 0x8, payload = payload.array())
  }

  private def encodeFrame(opcode: Int, payload: Array[Byte]): Array[Byte] = {
    val out = ByteArrayOutputStream()
    out.write(0x80 | (opcode & 0x0f))

    payload.length match {
      case len if len < 126 =>
        out.write(len)
      case len if len <= 0xffff =>
        out.write(126)
        out.write((len >>> 8) & 0xff)
        out.write(len & 0xff)
      case len =>
        out.write(127)
        val header = ByteBuffer.allocate(8)
        header.putLong(len.toLong)
        out.write(header.array())
    }

    out.write(payload)
    out.toByteArray
  }

  private def indexOf(haystack: Array[Byte], needle: Array[Byte]): Int = {
    if needle.isEmpty then 0
    else {
      var start = 0
      while start <= haystack.length - needle.length do
          var i     = 0
          var found = true
          while found && i < needle.length do
              found = haystack(start + i) == needle(i)
              i += 1
          if found then return start
          start += 1
      -1
    }
  }
}
