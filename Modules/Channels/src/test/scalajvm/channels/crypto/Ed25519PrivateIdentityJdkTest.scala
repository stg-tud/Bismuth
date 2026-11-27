package channels.crypto

import channels.crypto.jvm.Ed25519Util

import channels.crypto.vendored.Ed25519 as VendoredEd25519

/** Identities from JDK key pairs and from seeds are interchangeable. */
class Ed25519PrivateIdentityJdkTest extends munit.FunSuite {
  test("same key gives same public identity and signatures") {
    val fromKeyPair = IdentityFactory.fromIdentityKey(Ed25519Util.generateNewKeyPair)
    val fromSeed    = IdentityFactory.fromSeed(fromKeyPair.seedBytes)
    val data        = "some data".getBytes
    assertEquals(fromSeed.getPublic, fromKeyPair.getPublic)
    assertEquals(fromSeed.sign(data), fromKeyPair.sign(data))
    assertEquals(fromSeed.identityKey.getPublic.getEncoded.toSeq, fromKeyPair.getPublicKey.getEncoded.toSeq)
    assert(VendoredEd25519.verify(fromKeyPair.getPublic.rawPublicKeyBytes, data, fromKeyPair.sign(data).toArray))
  }
}
