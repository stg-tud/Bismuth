package lofi_acl.bft

import crypto.PublicIdentity
import lofi_acl.bft.HashDag.{Delta, Encoder, Hashable}

case class SignedDelta[State](
    signature: Signature,
    author: PublicIdentity,
    state: State,
    parents: Set[Hash]
) extends Delta[State]

object SignedDelta {
  given hashable[State](using encoder: Encoder[SignedDelta[State]]): Hashable[SignedDelta[State]] with
      override inline def hash(value: SignedDelta[State]): Hash = Hash.compute(encoder(value))
}
