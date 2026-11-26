package channels
package connection

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.util.Base64
import channels.crypto.vendored.Sha1

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
  val handshakePrefix: Int =
      val bytes = "GET ".getBytes(StandardCharsets.US_ASCII)
      ByteBuffer.wrap(bytes).getInt
  val headerEnd: Array[Byte] = "\r\n\r\n".getBytes(StandardCharsets.US_ASCII)
  val websocketGuid: String  = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

  final case class HandshakeRequest(path: String, headers: Map[String, String]) {
    def header(name: String): Option[String] = headers.get(name.toLowerCase)
  }

  /** Parses a complete handshake request (starting with `GET `) from `buffer`, advancing it past the request. */
  def tryParseHandshake(buffer: ByteBuffer): Option[HandshakeRequest] =
    tryTakeHeaderBlock(buffer).flatMap(parseHandshakeHeaderBytes)

  /** Takes the bytes up to an empty line from `buffer`, advancing it past the empty line, if there is one. */
  private def tryTakeHeaderBlock(buffer: ByteBuffer): Option[Array[Byte]] = {
    val start = buffer.position()
    val last  = buffer.limit() - headerEnd.length
    var end   = start
    while end <= last && !headerEnd.indices.forall(i => buffer.get(end + i) == headerEnd(i)) do end += 1
    if end > last then None
    else {
      val headerBytes = new Array[Byte](end - start)
      buffer.get(headerBytes)
      buffer.position(end + headerEnd.length)
      Some(headerBytes)
    }
  }

  /** The `Sec-WebSocket-Key` of our client handshakes (16 zero bytes). The key only shows that the server understands
    * websockets, it does not need to be random.
    */
  val clientKey: String = "AAAAAAAAAAAAAAAAAAAAAA=="

  /** The `Sec-WebSocket-Accept` value with which a server accepts `key`. */
  def acceptFor(key: String): String =
    Base64.getEncoder.encodeToString(Sha1.hash((key + websocketGuid).getBytes(StandardCharsets.UTF_8)))

  /** The handshake request of our client, for `path` on `host`. */
  def handshakeRequest(host: String, path: String): Array[Byte] =
    ("GET " + path + " HTTP/1.1\r\n" +
      s"Host: $host\r\n" +
      "Upgrade: websocket\r\n" +
      "Connection: Upgrade\r\n" +
      s"Sec-WebSocket-Key: $clientKey\r\n" +
      "Sec-WebSocket-Version: 13\r\n" +
      "\r\n").getBytes(StandardCharsets.US_ASCII)

  /** Parses a complete handshake response from `buffer`, advancing it past the response. Returns false if the
    * response is incomplete, throws if the server did not accept our handshake.
    */
  def tryParseHandshakeResponse(buffer: ByteBuffer): Boolean =
    tryTakeHeaderBlock(buffer) match
        case None        => false
        case Some(bytes) =>
          val lines   = new String(bytes, StandardCharsets.US_ASCII).split("\r\n").toList
          val status  = lines.headOption.getOrElse("")
          val headers = parseHeaderLines(lines.drop(1))
          if status.split(' ').lift(1) != Some("101") then
              throw IllegalStateException(s"websocket handshake rejected: $status")
          if headers.get("sec-websocket-accept") != Some(acceptFor(clientKey)) then
              throw IllegalStateException("websocket handshake: wrong Sec-WebSocket-Accept")
          true

  def handshakeResponse(request: HandshakeRequest): Array[Byte] = {
    val key = request.header("sec-websocket-key").getOrElse {
      throw IllegalArgumentException("missing Sec-WebSocket-Key header")
    }

    val accept = acceptFor(key)

    val response =
      "HTTP/1.1 101 Switching Protocols\r\n" +
      "Connection: Upgrade\r\n" +
      "Upgrade: websocket\r\n" +
      s"Sec-WebSocket-Accept: $accept\r\n" +
      "\r\n"

    response.getBytes(StandardCharsets.US_ASCII)
  }

  case class WebsocketHeader(len: Int, mask: Array[Byte], fin: Boolean, opcode: Int)

  def tryParseHeader(buffer: ByteBuffer): Option[WebsocketHeader] = {
    if buffer.remaining() < 2 then return None
    val firstByte  = buffer.get() & 0xff
    val secondByte = buffer.get() & 0xff

    val fin    = (firstByte & 0x80) != 0
    val opcode = firstByte & 0x0f
    val masked = (secondByte & 0x80) != 0
    val lenTag = secondByte & 0x7f

    val payloadLength: Long = lenTag match {
      case 126 =>
        if buffer.remaining() < 2 then return None
        val len = buffer.getShort
        len.toLong
      case 127 =>
        if buffer.remaining() < 8 then return None
        val len = buffer.getLong()
        len
      case other => other.toLong
    }

    if payloadLength < 0 || payloadLength > Int.MaxValue then
        throw IllegalStateException(s"unsupported websocket payload length: $payloadLength")

    val maskKeyLength = if masked then 4 else 0
    if buffer.remaining() < maskKeyLength then return None

    val maskKey =
        val arr = Array[Byte](0, 0, 0, 0)
        if masked then
            buffer.get(arr)
            ()
        arr

    Some(WebsocketHeader(len = payloadLength.toInt, mask = maskKey, fin = fin, opcode = opcode))
  }

  def tryParsePayload(header: WebsocketHeader, payload: Array[Byte]): WebsocketFrame = {

    if header.mask.exists(_ != 0) then {
      var i = 0
      while i < payload.length do
          payload(i) = (payload(i) ^ header.mask(i & 3)).toByte
          i += 1
    }

    header.opcode match {
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

  }

  def isFinalFrame(consumed: Int): Boolean = (consumed & 0x40000000) != 0
  def consumedBytes(consumed: Int): Int    = consumed & 0x3fffffff

  /** `fromClient`: whether the frame goes from the client to the server, see [[frameHeader]]. */
  def encodeBinaryFrame(payload: Array[Byte], fromClient: Boolean = false): Array[Byte] =
    encodeFrame(opcode = 0x2, payload = ByteBuffer.wrap(payload), fromClient)

  def encodePongFrame(payload: Array[Byte], fromClient: Boolean = false): Array[Byte] =
    encodeFrame(opcode = 0xa, payload = ByteBuffer.wrap(payload), fromClient)

  def encodeCloseFrame(code: Int = 1000, reason: String = "", fromClient: Boolean = false): Array[Byte] = {
    val reasonBytes = reason.getBytes(StandardCharsets.UTF_8)
    val payload     = ByteBuffer.allocate(2 + reasonBytes.length)
    payload.putShort(code.toShort)
    payload.put(reasonBytes)
    encodeFrame(opcode = 0x8, payload = payload.flip(), fromClient)
  }

  /** A final frame with the remaining bytes of `payload`. */
  def encodeFrame(opcode: Int, payload: ByteBuffer, fromClient: Boolean): Array[Byte] =
    frameHeader(opcode, payload.remaining(), fromClient) ++ MessageBuffer.convertByteBufferToArray(payload.duplicate())

  /** Header of a final frame with a payload of `length` bytes, sent by the client if `fromClient`, else by the server.
    *
    * The protocol requires frames from the client, and only those, to be "masked": the mask bit set and a 4-byte key
    * that the payload is XORed with. The key only obscures the payload from intermediaries for browser security and
    * receivers cannot check it, so we always use the all-zero key: the payload follows unchanged. Thus, the mask bit and
    * key just mark the direction client -> server.
    */
  def frameHeader(opcode: Int, length: Int, fromClient: Boolean = false): Array[Byte] = {
    val out     = ByteArrayOutputStream()
    val maskBit = if fromClient then 0x80 else 0
    out.write(0x80 | (opcode & 0x0f))

    length match {
      case len if len < 126 =>
        out.write(maskBit | len)
      case len if len <= 0xffff =>
        out.write(maskBit | 126)
        out.write((len >>> 8) & 0xff)
        out.write(len & 0xff)
      case len =>
        out.write(maskBit | 127)
        val header = ByteBuffer.allocate(8)
        header.putLong(len.toLong)
        out.write(header.array())
    }
    if fromClient then out.write(Array[Byte](0, 0, 0, 0))

    out.toByteArray
  }

  private def parseHandshakeHeaderBytes(headerBytes: Array[Byte]): Option[HandshakeRequest] = {
    val headerText = new String(headerBytes, StandardCharsets.US_ASCII)
    val lines      = headerText.split("\r\n").toList
    lines match {
      case requestLine :: rest =>
        val path = requestLine.drop(4).takeWhile(_ != ' ')
        Some(HandshakeRequest(path, parseHeaderLines(rest)))
      case _ => None
    }
  }

  /** HTTP header lines as a map from lower case names to values. */
  private def parseHeaderLines(lines: List[String]): Map[String, String] =
    lines.collect {
      case line if line.contains(":") =>
        val idx = line.indexOf(':')
        line.substring(0, idx).trim.toLowerCase -> line.substring(idx + 1).trim
    }.toMap

}
