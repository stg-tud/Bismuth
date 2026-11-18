package channels.crypto

import java.nio.charset.StandardCharsets

object Commitment {
  /** @param context bound into the commitment (fed into the digest first), e.g., the author of the event */
  def commit(context: Array[Byte], value: Array[Byte]): RevealedValue =
      val witness = Array.ofDim[Byte](32)
      SecureRandomBytes.fill(witness)
      RevealedValue(value, witness)

  def commit(context: String, value: Array[Byte]): RevealedValue =
    commit(context.getBytes(StandardCharsets.UTF_8), value)

  case class RevealedValue(value: Array[Byte], witness: Array[Byte]) {

    /** The commitment is only valid for the same context that was passed to [[commit]] */
    def commitment(context: Array[Byte]): Hash = Hash.computeConcatenated(context, witness, value)

    def commitment(context: String): Hash = commitment(context.getBytes(StandardCharsets.UTF_8))
  }
}
