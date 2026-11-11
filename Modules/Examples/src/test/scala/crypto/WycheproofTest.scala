package crypto

import crypto.WycheproofVectors.V
import munit.FunSuite

import scala.util.Try

class WycheproofTest extends FunSuite {
  def hex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  def run(
      vectors: List[V],
      seal: (Array[Byte], Array[Byte], Array[Byte], Array[Byte]) => Array[Byte],
      open: (Array[Byte], Array[Byte], Array[Byte], Array[Byte]) => Array[Byte]
  ): Unit =
    for v <- vectors do
      val (k, n, a, m) = (hex(v.key), hex(v.iv), hex(v.aad), hex(v.msg))
      val expected     = hex(v.ct) ++ hex(v.tag)
      if v.valid then
          assertEquals(seal(k, n, m, a).toSeq, expected.toSeq, s"tcId ${v.tcId}")
          assertEquals(open(k, n, expected, a).toSeq, m.toSeq, s"tcId ${v.tcId}")
      else assert(Try(open(k, n, expected, a)).isFailure, s"tcId ${v.tcId}")

  test("xchacha20_poly1305") {
    run(
      WycheproofVectors.xchacha20Poly1305,
      (k, n, m, a) => XChaCha20Poly1305.seal(k, n, m, a).drop(XChaCha20Poly1305.NonceSize),
      (k, n, c, a) => XChaCha20Poly1305.open(k, n ++ c, a)
    )
  }

  test("chacha20_poly1305 (RFC 8439 core)") {
    run(WycheproofVectors.chacha20Poly1305, XChaCha20Poly1305.sealIetf, XChaCha20Poly1305.openIetf)
  }
}
