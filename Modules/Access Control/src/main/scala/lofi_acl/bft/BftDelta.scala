package lofi_acl.bft

import crypto.PublicIdentity
import lofi_acl.bft.HashDag.{Delta, Encoder, Hashable}

case class BftDelta[State](
    signature: Signature,
    author: PublicIdentity,
    state: State,
    parents: Set[Hash]
) extends Delta[State]

object BftDelta {
  given hashable[State](using encoder: Encoder[BftDelta[State]]): Hashable[BftDelta[State]] with
      override inline def hash(value: BftDelta[State]): Hash = Hash.compute(encoder(value))
}
