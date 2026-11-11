package channels.experiments

import munit.FunSuite

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

class XChaCha20Poly1305Test extends FunSuite {
  def hex(s: String): Array[Byte] =
    s.filterNot(_.isWhitespace).grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
  def toHex(b: Array[Byte]): String = b.map(x => f"${x & 0xff}%02x").mkString

  // draft-irtf-cfrg-xchacha-03, appendix A.3.1
  val plaintext: Array[Byte] = "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."
    .getBytes(StandardCharsets.US_ASCII)
  val aad   = hex("50515253c0c1c2c3c4c5c6c7")
  val key   = hex("808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f")
  val nonce = hex("404142434445464748494a4b4c4d4e4f5051525354555657")
  val ciphertext = hex(
    """bd6d179d3e83d43b9576579493c0e939572a1700252bfaccbed2902c21396cbb
      |731c7f1b0b4aa6440bf3a82f4eda7e39ae64c6708c54c216cb96b72e1213b452
      |2f8c9ba40db5d945b11b69b982c1bb9e3f3fac2bc369488f76b2383565d3fff9
      |21f9664c97637da9768812f615c68b13b52e""".stripMargin
  )
  val tag = hex("c0875924c1c7987947deafd8780acf49")

  test("draft-irtf-cfrg-xchacha test vector") {
    val boxed = XChaCha20Poly1305.seal(key, nonce, plaintext, aad)
    assertEquals(toHex(boxed), toHex(nonce ++ ciphertext ++ tag))
    assertEquals(toHex(XChaCha20Poly1305.open(key, boxed, aad)), toHex(plaintext))
  }

  test("tampering is detected") {
    val boxed = XChaCha20Poly1305.seal(key, nonce, plaintext, aad)
    for i <- Seq(0, 30, boxed.length - 1) do
      val broken = boxed.clone()
      broken(i) = (broken(i) ^ 1).toByte
      intercept[SecurityException](XChaCha20Poly1305.open(key, broken, aad))
    intercept[SecurityException](XChaCha20Poly1305.open(key, boxed, Array[Byte](1)))
  }

  test("Aead roundtrip for various lengths") {
    val aead = XChaCha20Poly1305(XChaCha20Poly1305.generateKey())
    for len <- Seq(0, 1, 15, 16, 17, 63, 64, 65, 1000) do
      val msg = Array.tabulate[Byte](len)(_.toByte)
      val enc = aead.encrypt(ByteBuffer.wrap(msg), ByteBuffer.wrap(aad))
      val dec = aead.decrypt(enc, ByteBuffer.wrap(aad)).get
      val out = new Array[Byte](dec.remaining()); dec.get(out)
      assertEquals(toHex(out), toHex(msg))
  }
}
