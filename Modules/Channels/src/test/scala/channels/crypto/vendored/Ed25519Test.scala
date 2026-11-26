package channels.crypto.vendored

import channels.crypto.SecureRandomBytes

class Ed25519Test extends munit.FunSuite {
  def hex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  test("wycheproof eddsa_test.json") {
    for g <- Ed25519WycheproofVectors.groups do
        val (sk, pk) = (hex(g.sk), hex(g.pk))
        assertEquals(Ed25519.publicKey(sk).toSeq, pk.toSeq)
        for t <- g.tests do
            val (msg, sig) = (hex(t.msg), hex(t.sig))
            assertEquals(Ed25519.verify(pk, msg, sig), t.valid, s"tcId ${t.tcId}")
            if t.valid then assertEquals(Ed25519.sign(sk, msg).toSeq, sig.toSeq, s"tcId ${t.tcId}")
  }

  test("sign and verify random messages") {
    for len <- List(0, 1, 31, 64, 1000) do
        val seed = new Array[Byte](32)
        SecureRandomBytes.fill(seed)
        val msg = new Array[Byte](len)
        SecureRandomBytes.fill(msg)
        val pk  = Ed25519.publicKey(seed)
        val sig = Ed25519.sign(seed, msg)
        assert(Ed25519.verify(pk, msg, sig))
        sig(len % 64) = (sig(len % 64) ^ 1).toByte
        assert(!Ed25519.verify(pk, msg, sig))
  }

  test("malformed inputs are rejected") {
    val seed = new Array[Byte](32)
    val pk   = Ed25519.publicKey(seed)
    val sig  = Ed25519.sign(seed, Array.emptyByteArray)
    assert(!Ed25519.verify(pk.take(31), Array.emptyByteArray, sig))
    assert(!Ed25519.verify(pk, Array.emptyByteArray, sig.take(63)))
    assert(!Ed25519.verify(Array.fill(32)(0xff.toByte), Array.emptyByteArray, sig))
    intercept[IllegalArgumentException](Ed25519.sign(new Array[Byte](31), Array.emptyByteArray))
  }
}
