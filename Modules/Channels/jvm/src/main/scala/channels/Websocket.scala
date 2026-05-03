package channels

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64
import scala.util.chaining.scalaUtilChainingOps

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

  def tryParseHandshake(buffer: ByteBuffer): Option[HandshakeRequest] = {
    val start = buffer.position()
    val end   = buffer.array().indexOfSlice(headerEnd, start)
    if end < 0 then None
    else {
      val headerBytes = new Array[Byte](end - start)
      buffer.get(headerBytes)
      buffer.position(end + headerEnd.length)
      parseHandshakeHeaderBytes(headerBytes)
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

  private def parseHandshakeHeaderBytes(headerBytes: Array[Byte]): Option[HandshakeRequest] = {
    val headerText = new String(headerBytes, StandardCharsets.US_ASCII)
    val lines      = headerText.split("\r\n").toList
    lines match {
      case requestLine :: rest =>
        val path    = requestLine.drop(4).takeWhile(_ != ' ')
        val headers = rest.collect {
          case line if line.contains(":") =>
            val idx = line.indexOf(':')
            line.substring(0, idx).trim.toLowerCase -> line.substring(idx + 1).trim
        }.toMap
        Some(HandshakeRequest(path, headers))
      case _ => None
    }
  }

}
