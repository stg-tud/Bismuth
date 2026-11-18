package channels.crypto

import munit.FunSuite
import org.bouncycastle.jce.provider.BouncyCastleProvider

import java.security.*
import java.security.spec.PKCS8EncodedKeySpec

class Ed25519UtilTest extends FunSuite {
  if Security.getProvider("BC") == null then {
    Security.addProvider(new BouncyCastleProvider())
    ()
  }

  private val cryptoProviders = List("BC", "SunEC")

  test("generateNewKeyPair") {
    Ed25519Util.generateNewKeyPair
  }

  test("publicKeyToRawPublicKeyBytes should work with BC and SunEC providers") {
    cryptoProviders.foreach { keyGenProvider =>
      val keyPair = KeyPairGenerator.getInstance("Ed25519", keyGenProvider).generateKeyPair()
      val rawKey  = Ed25519Util.publicKeyToRawPublicKeyBytes(keyPair.getPublic)
      assertEquals(rawKey.length, 32)

      cryptoProviders.foreach { verifyProvider =>
        val parsedPubKey = Ed25519Util.rawPublicKeyBytesToPublicKey(rawKey)
        testKeyEqualityUsingSignature(parsedPubKey, keyPair.getPrivate, keyGenProvider, verifyProvider)
      }
    }
  }

  test("privateKeyToRawPrivateKeyBytes should work with BC and SunEC providers") {
    cryptoProviders.foreach { providerName =>
      val gen     = KeyPairGenerator.getInstance("Ed25519", providerName)
      val keyPair = gen.generateKeyPair()
      val rawKey  = Ed25519Util.privateKeyToRawPrivateKeyBytes(keyPair.getPrivate)
      assertEquals(rawKey.length, 32)

      cryptoProviders.foreach { otherProvider =>
        val parsedKey: PrivateKey = Ed25519Util.rawPrivateKeyBytesToPrivateKey(rawKey)
        testKeyEqualityUsingSignature(
          keyPair.getPublic,
          parsedKey,
          signProvider = otherProvider,
          verifyProvider = providerName
        )
      }
    }
  }

  test("rawPrivateKeyBytesToKeyPair should work with BC and SunEC providers") {
    cryptoProviders.foreach { providerName =>
      val gen     = KeyPairGenerator.getInstance("Ed25519", providerName)
      val keyPair = gen.generateKeyPair()
      val rawKey  = Ed25519Util.privateKeyToRawPrivateKeyBytes(keyPair.getPrivate)
      assertEquals(rawKey.length, 32)

      cryptoProviders.foreach { otherProvider =>
        val parsedKeyPair = Ed25519Util.rawPrivateKeyBytesToKeyPair(rawKey)
        testKeyEqualityUsingSignature(
          keyPair.getPublic,
          parsedKeyPair.getPrivate,
          signProvider = otherProvider,
          verifyProvider = providerName
        )
        testKeyEqualityUsingSignature(
          parsedKeyPair.getPublic,
          keyPair.getPrivate,
          signProvider = otherProvider,
          verifyProvider = providerName
        )
      }
    }
  }

  test("privateKeyToPkcs8EncodedPrivateKeyBytes should work with BC and SunEC providers") {
    cryptoProviders.foreach { providerName =>
      val gen               = KeyPairGenerator.getInstance("Ed25519", providerName)
      val keyPair           = gen.generateKeyPair()
      val encodedPrivateKey = Ed25519Util.privateKeyToPkcs8EncodedPrivateKeyBytes(keyPair.getPrivate)

      cryptoProviders.foreach { otherProvider =>
        val spec                  = new PKCS8EncodedKeySpec(encodedPrivateKey, "Ed25519")
        val parsedKey: PrivateKey = KeyFactory.getInstance("Ed25519", otherProvider).generatePrivate(spec)
        testKeyEqualityUsingSignature(
          keyPair.getPublic,
          parsedKey,
          signProvider = otherProvider,
          verifyProvider = providerName
        )
      }
    }
  }

  test("testEqualityUsingSignature should work with same key") {
    cryptoProviders.flatMap(a => cryptoProviders.map(b => (a, b))).foreach { (providerA, providerB) =>
      val keyA = Ed25519Util.generateNewKeyPair
      testKeyEqualityUsingSignature(keyA.getPublic, keyA.getPrivate, providerA, providerB)
    }
  }

  test("testEqualityUsingSignature should fail with different key") {
    cryptoProviders.flatMap(a => cryptoProviders.map(b => (a, b))).foreach { (providerA, providerB) =>
      val keyA = Ed25519Util.generateNewKeyPair
      val keyB = Ed25519Util.generateNewKeyPair
      intercept[AssertionError] {
        testKeyEqualityUsingSignature(keyA.getPublic, keyB.getPrivate, providerA, providerB)
      }
    }
  }

//  private def testKeyEqualityUsingSignatureWithAllProviders(keyPairA: KeyPair, keyPairB: KeyPair): Unit = {
//    cryptoProviders.foreach { providerNameVerify =>
//      cryptoProviders.foreach { providerNameSign =>
//        testKeyEqualityUsingSignature(keyPairA.getPublic, keyPairB.getPrivate, providerNameSign, providerNameVerify)
//        testKeyEqualityUsingSignature(keyPairB.getPublic, keyPairA.getPrivate, providerNameSign, providerNameVerify)
//      }
//    }
//  }

  private def testKeyEqualityUsingSignature(
      pubKey: PublicKey,
      privateKey: PrivateKey,
      signProvider: String,
      verifyProvider: String
  ): Unit = {
    val signer = Signature.getInstance("Ed25519", signProvider)
    signer.initSign(privateKey)
    signer.update("Test".getBytes())
    val signature = signer.sign()

    val verifier = Signature.getInstance("Ed25519", verifyProvider)
    verifier.initVerify(pubKey)
    verifier.update("Test".getBytes())
    assert(verifier.verify(signature))
  }
}
