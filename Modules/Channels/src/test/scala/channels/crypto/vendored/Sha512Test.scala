package channels.crypto.vendored

import java.nio.charset.StandardCharsets

class Sha512Test extends munit.FunSuite {

  def hex(bytes: Array[Byte]): String = bytes.map(b => f"${b & 0xff}%02x").mkString

  def ascii(s: String): Array[Byte] = s.getBytes(StandardCharsets.US_ASCII)

  // test vectors from FIPS 180-2 / NIST examples
  val vectors: List[(Array[Byte], String)] = List(
    ascii("") ->
    "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e",
    ascii("abc") ->
    "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f",
    ascii(
      "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
    ) ->
    "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909",
    Array.fill[Byte](1000000)('a'.toByte) ->
    "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b",
  )

  test("test vectors") {
    vectors.foreach((message, expected) => assertEquals(hex(Sha512.hash(message)), expected))
  }

  test("incremental updates") {
    vectors.foreach { (message, expected) =>
      for chunkSize <- List(1, 37, 128, 200) do
          val sha = Sha512()
          message.grouped(chunkSize).foreach(chunk => sha.update(chunk))
          assertEquals(hex(sha.digest()), expected, s"chunk size $chunkSize")
    }
  }
}
