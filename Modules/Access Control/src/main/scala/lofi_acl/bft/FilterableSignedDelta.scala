package lofi_acl.bft

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.HashDag.{Delta, Encoder}
import rdts.base.Bottom

case class FilterableSignedDelta[RDT: Bottom](
    author: PublicIdentity,
    signature: Signature,
    parents: Set[Hash],
    payload: Option[RDT]
)(using encoder: Encoder[FilterableSignedDelta[RDT]]) extends Delta[RDT] {
  def hash(): Hash = Hash.compute(encoder(this.filtered))
  // TODO: Replace with method that applies filter according to the specified permissions
  def filtered: FilterableSignedDelta[RDT] = this.copy(payload = None)
  def isValid: Boolean                     = signature.verify(author.publicKey, encoder(this.copy(signature = null)))
  def rdt: RDT                             = payload.getOrElse(Bottom[RDT].empty)
}

object FilterableSignedDelta {
  def apply[RDT: Bottom](
      authorKey: PrivateIdentity,
      parents: Set[Hash],
      rdt: RDT
  )(using encoder: Encoder[FilterableSignedDelta[RDT]]): FilterableSignedDelta[RDT] = {
    val delta = FilterableSignedDelta(
      authorKey.getPublic,
      null,
      parents,
      Some(rdt)
    )
    delta.copy(signature = Signature.compute(encoder(delta), authorKey.identityKey.getPrivate))
  }
}
