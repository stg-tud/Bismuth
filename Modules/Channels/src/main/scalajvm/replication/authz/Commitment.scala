package replication.authz

import replication.acl.bft.Hash

import java.security.{MessageDigest, SecureRandom}

object Commitment {
  private val random = SecureRandom()

  def commit(value: Array[Byte]): RevealedValue =
      val witness = Array.ofDim[Byte](32)
      random.nextBytes(witness)
      RevealedValue(value, witness)

  case class RevealedValue(value: Array[Byte], witness: Array[Byte]) {
    def commitment: Hash =
        val digest = MessageDigest.getInstance("SHA3-256", "SUN")
        digest.update(witness)
        digest.update(value)
        Hash.unsafeFromArray(digest.digest())
  }
}
