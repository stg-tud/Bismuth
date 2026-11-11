package crypto

import channels.connection.MessageBuffer
import channels.experiments.Aead

import java.nio.ByteBuffer
import java.security.SecureRandom
import scala.util.Try

/** Minimal XChaCha20-Poly1305 AEAD (draft-irtf-cfrg-xchacha, RFC 8439) for JVM and Scala.js.
  *
  * Ciphertext layout is `nonce (24) ‖ ciphertext ‖ tag (16)`, compatible with Tink's raw XChaCha20Poly1305.
  * Nonces are random, which is safe due to the 192-bit nonce size.
  *
  * Ported down from Tink (Apache 2.0); Poly1305 is based on poly1305-donna (public domain).
  * Not hardened against side channels beyond what the original does; for experiments only.
  */
class XChaCha20Poly1305(key: Array[Byte]) extends Aead {
  import XChaCha20Poly1305.*
  require(key.length == KeySize, s"key must be $KeySize bytes")

  override def encrypt(plain: ByteBuffer, associated: ByteBuffer): ByteBuffer =
    val nonce = new Array[Byte](NonceSize)
    random.nextBytes(nonce)
    ByteBuffer.wrap(seal(key, nonce, MessageBuffer.convertByteBufferToArray(plain), MessageBuffer.convertByteBufferToArray(associated)))

  override def decrypt(cypher: ByteBuffer, associated: ByteBuffer): Try[ByteBuffer] =
    Try(ByteBuffer.wrap(open(key, MessageBuffer.convertByteBufferToArray(cypher), MessageBuffer.convertByteBufferToArray(associated))))
}

object XChaCha20Poly1305 {
  val KeySize   = 32
  val NonceSize = 24
  val TagSize   = 16

  private lazy val random = new SecureRandom()

  def generateKey(): Array[Byte] =
    val k = new Array[Byte](KeySize)
    random.nextBytes(k)
    k

  /** Returns `nonce ‖ ciphertext ‖ tag`. */
  def seal(key: Array[Byte], nonce: Array[Byte], plain: Array[Byte], aad: Array[Byte]): Array[Byte] =
    require(nonce.length == NonceSize)
    val (subkey, nonce12) = derive(key, nonce)
    val out               = new Array[Byte](NonceSize + plain.length + TagSize)
    System.arraycopy(nonce, 0, out, 0, NonceSize)
    ChaCha20.xor(subkey, nonce12, 1, plain, 0, plain.length, out, NonceSize)
    val tag = Poly1305.mac(polyKey(subkey, nonce12), macData(aad, out, NonceSize, plain.length))
    System.arraycopy(tag, 0, out, NonceSize + plain.length, TagSize)
    out

  /** Inverse of [[seal]]; throws on authentication failure. */
  def open(key: Array[Byte], boxed: Array[Byte], aad: Array[Byte]): Array[Byte] =
    if boxed.length < NonceSize + TagSize then throw IllegalArgumentException("ciphertext too short")
    val nonce             = java.util.Arrays.copyOfRange(boxed, 0, NonceSize)
    val ctLen             = boxed.length - NonceSize - TagSize
    val (subkey, nonce12) = derive(key, nonce)
    val expected          = Poly1305.mac(polyKey(subkey, nonce12), macData(aad, boxed, NonceSize, ctLen))
    var diff              = 0
    for i <- 0 until TagSize do diff |= expected(i) ^ boxed(NonceSize + ctLen + i)
    if diff != 0 then throw SecurityException("invalid tag")
    val out = new Array[Byte](ctLen)
    ChaCha20.xor(subkey, nonce12, 1, boxed, NonceSize, ctLen, out, 0)
    out

  /** HChaCha20 subkey from first 16 nonce bytes; remaining 8 bytes form the 12 byte ChaCha20 nonce. */
  private def derive(key: Array[Byte], nonce: Array[Byte]): (Array[Int], Array[Int]) =
    val subkey  = ChaCha20.hChaCha20(ChaCha20.toInts(key, 0, 8), ChaCha20.toInts(nonce, 0, 4))
    val nonce12 = Array(0, ChaCha20.load32(nonce, 16), ChaCha20.load32(nonce, 20))
    (subkey, nonce12)

  private def polyKey(key: Array[Int], nonce: Array[Int]): Array[Byte] =
    val out = new Array[Byte](32)
    ChaCha20.xor(key, nonce, 0, new Array[Byte](32), 0, 32, out, 0)
    out

  /** RFC 8439 §2.8: aad ‖ pad16 ‖ ct ‖ pad16 ‖ le64(aadLen) ‖ le64(ctLen). */
  private def macData(aad: Array[Byte], ct: Array[Byte], ctOff: Int, ctLen: Int): Array[Byte] =
    def padded(n: Int) = (n + 15) & ~15
    val aadEnd         = padded(aad.length)
    val ctEnd          = aadEnd + padded(ctLen)
    val buf            = new Array[Byte](ctEnd + 16)
    System.arraycopy(aad, 0, buf, 0, aad.length)
    System.arraycopy(ct, ctOff, buf, aadEnd, ctLen)
    ChaCha20.store64(buf, ctEnd, aad.length.toLong)
    ChaCha20.store64(buf, ctEnd + 8, ctLen.toLong)
    buf
}

private[crypto] object ChaCha20 {
  // "expand 32-byte k"
  private val Sigma = Array(0x61707865, 0x3320646e, 0x79622d32, 0x6b206574)

  def load32(b: Array[Byte], i: Int): Int =
    (b(i) & 0xff) | ((b(i + 1) & 0xff) << 8) | ((b(i + 2) & 0xff) << 16) | ((b(i + 3) & 0xff) << 24)

  def store64(b: Array[Byte], i: Int, v: Long): Unit =
    for j <- 0 until 8 do b(i + j) = (v >>> (8 * j)).toByte

  def toInts(b: Array[Byte], off: Int, n: Int): Array[Int] = Array.tabulate(n)(i => load32(b, off + 4 * i))

  private def initState(key: Array[Int]): Array[Int] =
    val s = new Array[Int](16)
    System.arraycopy(Sigma, 0, s, 0, 4)
    System.arraycopy(key, 0, s, 4, 8)
    s

  private def quarterRound(x: Array[Int], a: Int, b: Int, c: Int, d: Int): Unit =
    x(a) += x(b); x(d) = Integer.rotateLeft(x(d) ^ x(a), 16)
    x(c) += x(d); x(b) = Integer.rotateLeft(x(b) ^ x(c), 12)
    x(a) += x(b); x(d) = Integer.rotateLeft(x(d) ^ x(a), 8)
    x(c) += x(d); x(b) = Integer.rotateLeft(x(b) ^ x(c), 7)

  private def rounds(x: Array[Int]): Unit =
    for _ <- 0 until 10 do
      quarterRound(x, 0, 4, 8, 12); quarterRound(x, 1, 5, 9, 13)
      quarterRound(x, 2, 6, 10, 14); quarterRound(x, 3, 7, 11, 15)
      quarterRound(x, 0, 5, 10, 15); quarterRound(x, 1, 6, 11, 12)
      quarterRound(x, 2, 7, 8, 13); quarterRound(x, 3, 4, 9, 14)

  /** HChaCha20: key (8 words) and 16 byte nonce (4 words) to 256 bit subkey (8 words). */
  def hChaCha20(key: Array[Int], nonce: Array[Int]): Array[Int] =
    val s = initState(key)
    System.arraycopy(nonce, 0, s, 12, 4)
    rounds(s)
    Array(s(0), s(1), s(2), s(3), s(12), s(13), s(14), s(15))

  /** XORs `len` bytes of `in` with the RFC 8439 ChaCha20 keystream starting at block `counter`. */
  def xor(key: Array[Int], nonce: Array[Int], counter: Int, in: Array[Byte], inOff: Int, len: Int, out: Array[Byte], outOff: Int): Unit =
    val state = initState(key)
    System.arraycopy(nonce, 0, state, 13, 3)
    val x   = new Array[Int](16)
    var pos = 0
    var ctr = counter
    while pos < len do
      state(12) = ctr
      System.arraycopy(state, 0, x, 0, 16)
      rounds(x)
      var i = 0
      while i < 64 && pos < len do
        val word = x(i >> 2) + state(i >> 2)
        out(outOff + pos) = (in(inOff + pos) ^ (word >>> (8 * (i & 3)))).toByte
        i += 1
        pos += 1
      ctr += 1
}

/** Poly1305 one-time MAC (RFC 8439), 26-bit limb variant of poly1305-donna. */
private[crypto] object Poly1305 {
  private def load32(in: Array[Byte], i: Int): Long = ChaCha20.load32(in, i) & 0xffffffffL
  private def load26(in: Array[Byte], i: Int, shift: Int): Long = (load32(in, i) >> shift) & 0x3ffffff

  def mac(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    val r0 = load26(key, 0, 0)
    val r1 = load26(key, 3, 2) & 0x3ffff03
    val r2 = load26(key, 6, 4) & 0x3ffc0ff
    val r3 = load26(key, 9, 6) & 0x3f03fff
    val r4 = load26(key, 12, 8) & 0x00fffff
    val s1 = r1 * 5; val s2 = r2 * 5; val s3 = r3 * 5; val s4 = r4 * 5

    var h0, h1, h2, h3, h4 = 0L
    var c                  = 0L
    val buf                = new Array[Byte](17)
    var i                  = 0
    while i < data.length do
      val n = math.min(16, data.length - i)
      java.util.Arrays.fill(buf, 0.toByte)
      System.arraycopy(data, i, buf, 0, n)
      buf(n) = 1
      h0 += load26(buf, 0, 0)
      h1 += load26(buf, 3, 2)
      h2 += load26(buf, 6, 4)
      h3 += load26(buf, 9, 6)
      h4 += load26(buf, 12, 8) | (buf(16).toLong << 24)

      val d0 = h0 * r0 + h1 * s4 + h2 * s3 + h3 * s2 + h4 * s1
      var d1 = h0 * r1 + h1 * r0 + h2 * s4 + h3 * s3 + h4 * s2
      var d2 = h0 * r2 + h1 * r1 + h2 * r0 + h3 * s4 + h4 * s3
      var d3 = h0 * r3 + h1 * r2 + h2 * r1 + h3 * r0 + h4 * s4
      var d4 = h0 * r4 + h1 * r3 + h2 * r2 + h3 * r1 + h4 * r0

      c = d0 >> 26; h0 = d0 & 0x3ffffff; d1 += c
      c = d1 >> 26; h1 = d1 & 0x3ffffff; d2 += c
      c = d2 >> 26; h2 = d2 & 0x3ffffff; d3 += c
      c = d3 >> 26; h3 = d3 & 0x3ffffff; d4 += c
      c = d4 >> 26; h4 = d4 & 0x3ffffff
      h0 += c * 5; c = h0 >> 26; h0 &= 0x3ffffff; h1 += c
      i += 16

    // full carry
    c = h1 >> 26; h1 &= 0x3ffffff; h2 += c
    c = h2 >> 26; h2 &= 0x3ffffff; h3 += c
    c = h3 >> 26; h3 &= 0x3ffffff; h4 += c
    c = h4 >> 26; h4 &= 0x3ffffff; h0 += c * 5
    c = h0 >> 26; h0 &= 0x3ffffff; h1 += c

    // g = h - p; select h if h < p
    var g0 = h0 + 5; c = g0 >> 26; g0 &= 0x3ffffff
    var g1 = h1 + c; c = g1 >> 26; g1 &= 0x3ffffff
    var g2 = h2 + c; c = g2 >> 26; g2 &= 0x3ffffff
    var g3 = h3 + c; c = g3 >> 26; g3 &= 0x3ffffff
    val g4   = h4 + c - (1L << 26)
    val mask = g4 >> 63
    h0 = (h0 & mask) | (g0 & ~mask)
    h1 = (h1 & mask) | (g1 & ~mask)
    h2 = (h2 & mask) | (g2 & ~mask)
    h3 = (h3 & mask) | (g3 & ~mask)
    h4 = (h4 & mask) | (g4 & ~mask)

    // h mod 2^128, then + s
    h0 = (h0 | (h1 << 26)) & 0xffffffffL
    h1 = ((h1 >> 6) | (h2 << 20)) & 0xffffffffL
    h2 = ((h2 >> 12) | (h3 << 14)) & 0xffffffffL
    h3 = ((h3 >> 18) | (h4 << 8)) & 0xffffffffL

    val out = new Array[Byte](16)
    c = 0L
    for (h, j) <- Seq(h0, h1, h2, h3).zipWithIndex do
      c = h + load32(key, 16 + 4 * j) + (c >> 32)
      for k <- 0 until 4 do out(4 * j + k) = (c >>> (8 * k)).toByte
    out
}
