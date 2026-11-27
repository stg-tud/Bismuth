package channels.crypto

import channels.crypto.vendored.Ed25519 as VendoredEd25519

class Ed25519PrivateIdentityTest extends munit.FunSuite {
  def hex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  test("RFC 8032 test 2") {
    val id = IdentityFactory.fromSeed(hex("4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"))
    assertEquals(
      id.getPublic.rawPublicKeyBytes.toSeq,
      hex("3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c").toSeq
    )
    assertEquals(
      id.sign(hex("72")).toArray.toSeq,
      hex(
        "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"
      ).toSeq
    )
  }

  test("platform signer and verifier agree with the vendored implementation") {
    val id   = IdentityFactory.createNewIdentity
    val data = "hello".getBytes
    val sig  = id.sign(data)
    assertEquals(sig.toArray.toSeq, VendoredEd25519.sign(id.seedBytes, data).toSeq)
    assert(Ed25519.verifier.verify(id.getPublic, data, sig))
    assert(!Ed25519.verifier.verify(id.getPublic, "hellO".getBytes, sig))
    assert(!Ed25519.verifier.verify(IdentityFactory.createNewIdentity.getPublic, data, sig))
  }

  test("seed round trip") {
    val id = IdentityFactory.createNewIdentity
    assertEquals(IdentityFactory.fromSeed(id.seedBytes).getPublic, id.getPublic)
  }
}
