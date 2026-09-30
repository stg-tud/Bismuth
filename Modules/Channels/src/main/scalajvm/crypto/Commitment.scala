package crypto

import crypto.Hash

import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, SecureRandom}

object Commitment {
  private val random = SecureRandom()

  /** @param context bound into the commitment (fed into the digest first), e.g., the author of the event */
  def commit(context: Array[Byte], value: Array[Byte]): RevealedValue =
      val witness = Array.ofDim[Byte](32)
      random.nextBytes(witness)
      RevealedValue(value, witness)

  def commit(context: String, value: Array[Byte]): RevealedValue =
    commit(context.getBytes(StandardCharsets.UTF_8), value)

  case class RevealedValue(value: Array[Byte], witness: Array[Byte]) {

    /** The commitment is only valid for the same context that was passed to [[commit]] */
    def commitment(context: Array[Byte]): Hash =
        val digest = MessageDigest.getInstance("SHA3-256", "SUN")
        digest.update(context)
        digest.update(witness)
        digest.update(value)
        Hash.unsafeFromArray(digest.digest())

    def commitment(context: String): Hash = commitment(context.getBytes(StandardCharsets.UTF_8))
  }
}
