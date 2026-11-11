package channels.experiments

import munit.FunSuite
import org.bouncycastle.crypto.modes.ChaCha20Poly1305 as BcChaCha20Poly1305
import org.bouncycastle.crypto.macs.Poly1305 as BcPoly1305
import org.bouncycastle.crypto.params.{AEADParameters, KeyParameter}

import scala.util.Random

/** Differential tests against BouncyCastle with random and adversarial (all 0x00 / 0xff) inputs. */
class BouncyCastleCompatTest extends FunSuite {
  val rnd = Random(42)

  def bytes(n: Int, mode: Int): Array[Byte] = mode match
    case 0 => Array.fill(n)(0.toByte)
    case 1 => Array.fill(n)(0xff.toByte)
    case _ => Array.fill(n)(rnd.nextInt().toByte)

  def bcPoly(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    val mac = BcPoly1305()
    mac.init(KeyParameter(key))
    mac.update(data, 0, data.length)
    val out = new Array[Byte](16)
    mac.doFinal(out, 0)
    out

  def bcAead(key: Array[Byte], nonce: Array[Byte], plain: Array[Byte], aad: Array[Byte]): Array[Byte] =
    val c = BcChaCha20Poly1305()
    c.init(true, AEADParameters(KeyParameter(key), 128, nonce, aad))
    val out = new Array[Byte](c.getOutputSize(plain.length))
    val n   = c.processBytes(plain, 0, plain.length, out, 0)
    c.doFinal(out, n)
    out

  test("Poly1305 matches BouncyCastle") {
    for
      keyMode  <- 0 to 2
      dataMode <- 0 to 2
      len      <- Seq(0, 1, 15, 16, 17, 31, 32, 33, 64, 255, 1000)
      _        <- 0 until 5
    do
      val key  = bytes(32, keyMode)
      val data = bytes(len, dataMode)
      assertEquals(Poly1305.mac(key, data).toSeq, bcPoly(key, data).toSeq, s"key=$keyMode data=$dataMode len=$len")
  }

  test("ChaCha20-Poly1305 matches BouncyCastle") {
    for
      mode <- 0 to 2
      len  <- Seq(0, 1, 63, 64, 65, 127, 128, 129, 4096)
      _    <- 0 until 3
    do
      val (k, n, m, a) = (bytes(32, mode), bytes(12, mode), bytes(len, mode), bytes(len % 37, mode))
      val ours         = XChaCha20Poly1305.sealIetf(k, n, m, a)
      assertEquals(ours.toSeq, bcAead(k, n, m, a).toSeq)
      assertEquals(XChaCha20Poly1305.openIetf(k, n, ours, a).toSeq, m.toSeq)
  }
}
