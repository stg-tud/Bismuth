package channels.experiments

import munit.FunSuite

class XChaCha20Poly1305TinkCompatTest extends FunSuite {
  test("interoperable with Tink subtle XChaCha20Poly1305") {
    val key  = XChaCha20Poly1305.generateKey()
    val tink = com.google.crypto.tink.subtle.XChaCha20Poly1305(key)
    for len <- Seq(0, 1, 16, 63, 64, 65, 4096) do
      val msg = Array.tabulate[Byte](len)(i => (i * 7).toByte)
      val aad = Array.tabulate[Byte](len % 23)(_.toByte)
      assertEquals(XChaCha20Poly1305.open(key, tink.encrypt(msg, aad), aad).toSeq, msg.toSeq)
      val nonce = XChaCha20Poly1305.generateKey().take(24)
      assertEquals(tink.decrypt(XChaCha20Poly1305.seal(key, nonce, msg, aad), aad).toSeq, msg.toSeq)
  }
}
