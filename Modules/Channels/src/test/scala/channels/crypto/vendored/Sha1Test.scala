package channels.crypto.vendored

import java.nio.charset.StandardCharsets

class Sha1Test extends munit.FunSuite {

  def hex(bytes: Array[Byte]): String = bytes.map(b => f"${b & 0xff}%02x").mkString

  def ascii(s: String): Array[Byte] = s.getBytes(StandardCharsets.US_ASCII)

  // test vectors from FIPS 180 / RFC 3174
  val vectors: List[(Array[Byte], String)] = List(
    ascii("")                                                         -> "da39a3ee5e6b4b0d3255bfef95601890afd80709",
    ascii("abc")                                                      -> "a9993e364706816aba3e25717850c26c9cd0d89d",
    ascii("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq") ->
    "84983e441c3bd26ebaae4aa1f95129e5e54670f1",
    Array.fill[Byte](1000000)('a'.toByte) -> "34aa973cd4c4daa4f61eeb2bdbad27316534016f",
  )

  test("test vectors") {
    vectors.foreach((message, expected) => assertEquals(hex(Sha1.hash(message)), expected))
  }

  test("incremental updates") {
    vectors.foreach { (message, expected) =>
      val sha1 = Sha1()
      message.grouped(37).foreach(chunk => sha1.update(chunk, 0, chunk.length))
      val hashed = new Array[Byte](Sha1.HASH_SIZE)
      sha1.finish(hashed, 0)
      assertEquals(hex(hashed), expected)
    }
  }
}
