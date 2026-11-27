package ex2026accessControl.acl

import channels.crypto.{Ed25519, Hash, PrivateIdentity, PublicIdentity, Signature}
import ex2026accessControl.acl.HashDag.{Delta, Encoder, Hashable}
import rdts.base.Bottom

case class BftFilterableSignedDelta[RDT: Bottom](
    author: PublicIdentity,
    signature: Signature | Null,
    parents: Set[Hash],
    payload: Option[RDT]
) extends Delta[RDT] {
  // TODO: Replace with method that applies filter according to the specified permissions
  def filtered: BftFilterableSignedDelta[RDT]                                          = this.copy(payload = None)
  def isSignatureValid(using encoder: Encoder[BftFilterableSignedDelta[RDT]]): Boolean =
    Ed25519.verifier.verify(author, encoder(this.copy(signature = null)), signature.nn)
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
    delta.copy(signature = authorKey.sign(encoder(delta)))
  }

  given hashable[RDT](using encoder: Encoder[BftFilterableSignedDelta[RDT]]): Hashable[BftFilterableSignedDelta[RDT]]
  with
      override def hash(value: BftFilterableSignedDelta[RDT]): Hash = Hash.compute(encoder(value.filtered))

}
