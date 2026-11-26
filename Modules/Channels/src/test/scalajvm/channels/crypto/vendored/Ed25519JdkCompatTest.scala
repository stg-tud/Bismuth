package channels.crypto.vendored

import channels.crypto.Ed25519Util

import scala.util.Random

/** Differential test against the JDK (SunEC) implementation. */
class Ed25519JdkCompatTest extends munit.FunSuite {

  test("identical keys and signatures, mutual verification") {
    val random = Random(42)
    for i <- 0 until 50 do
        val keyPair = Ed25519Util.generateNewKeyPair
        val seed    = Ed25519Util.privateKeyToRawPrivateKeyBytes(keyPair.getPrivate)
        val jdkPk   = Ed25519Util.publicKeyToRawPublicKeyBytes(keyPair.getPublic)
        val msg     = random.nextBytes(random.nextInt(300))

        assertEquals(Ed25519.publicKey(seed).toSeq, jdkPk.toSeq)

        val jdkSig = Ed25519Util.sign(msg, keyPair.getPrivate)
        val ourSig = Ed25519.sign(seed, msg)
        assertEquals(ourSig.toSeq, jdkSig.toSeq)
        assert(Ed25519.verify(jdkPk, msg, jdkSig))
        assert(Ed25519Util.checkEd25519Signature(msg, ourSig, keyPair.getPublic))
  }
}
