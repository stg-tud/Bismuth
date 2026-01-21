package lofi_acl.bft

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.HashDag.{Delta, Encoder, Hashable}
import rdts.base.Bottom

case class FilterableSignedDelta[RDT: Bottom](
    author: PublicIdentity,
    signature: Signature,
    parents: Set[Hash],
    payload: Option[RDT]
) extends Delta[RDT] {
  // TODO: Replace with method that applies filter according to the specified permissions
  def filtered: FilterableSignedDelta[RDT]                                          = this.copy(payload = None)
  def isSignatureValid(using encoder: Encoder[FilterableSignedDelta[RDT]]): Boolean =
    signature.verify(author.publicKey, encoder(this.copy(signature = null)))
  override def rdt: RDT = payload.getOrElse(Bottom[RDT].empty)
}

object FilterableSignedDelta {
  def fromDelta[RDT: Bottom](
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

  given hashable[RDT](using encoder: Encoder[FilterableSignedDelta[RDT]]): Hashable[FilterableSignedDelta[RDT]] with
      override inline def hash(value: FilterableSignedDelta[RDT]): Hash = Hash.compute(encoder(value.filtered))

}
