package replication.acl

import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity, Signature}
import rdts.base.Bottom
import replication.HashDag.{Delta, Encoder, Hashable}

case class BftFilterableSignedDelta[RDT: Bottom](
    author: PublicIdentity,
    signature: Signature | Null,
    parents: Set[Hash],
    payload: Option[RDT]
) extends Delta[RDT] {
  // TODO: Replace with method that applies filter according to the specified permissions
  def filtered: BftFilterableSignedDelta[RDT]                                          = this.copy(payload = None)
  def isSignatureValid(using encoder: Encoder[BftFilterableSignedDelta[RDT]]): Boolean =
    signature.nn.verify(author.publicKey, encoder(this.copy(signature = null)))
  override def state: RDT = payload.getOrElse(Bottom[RDT].empty)
}

object BftFilterableSignedDelta {
  def fromDelta[RDT: Bottom](
      authorKey: PrivateIdentity,
      parents: Set[Hash],
      rdt: RDT
  )(using encoder: Encoder[BftFilterableSignedDelta[RDT]]): BftFilterableSignedDelta[RDT] = {
    val delta = BftFilterableSignedDelta(
      authorKey.getPublic,
      null,
      parents,
      Some(rdt)
    )
    delta.copy(signature = Signature.compute(encoder(delta), authorKey.identityKey.getPrivate))
  }

  given hashable[RDT](using encoder: Encoder[BftFilterableSignedDelta[RDT]]): Hashable[BftFilterableSignedDelta[RDT]]
  with
      override def hash(value: BftFilterableSignedDelta[RDT]): Hash = Hash.compute(encoder(value.filtered))

}
