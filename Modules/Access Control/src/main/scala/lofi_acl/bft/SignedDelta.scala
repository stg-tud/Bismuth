package lofi_acl.bft

import crypto.PublicIdentity
import lofi_acl.bft.HashDag.{Delta, Encoder, Hashable}

import java.security.MessageDigest

case class SignedDelta[RDT](
    signature: Signature,
    author: PublicIdentity,
    rdt: RDT,
    parents: Set[Hash]
) extends Delta[RDT]

object SignedDelta {
  given hashable[RDT](using encoder: Encoder[SignedDelta[RDT]]): Hashable[SignedDelta[RDT]] with
      override def hash(value: SignedDelta[RDT]): Hash = Hash.compute(encoder(value))
}
