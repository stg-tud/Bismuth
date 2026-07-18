package replication.acl.sync.anti_entropy

import crypto.{PublicIdentity, Signature}
import crypto.channels.PrivateIdentity
import replication.HashDag.Encoder
import rdts.time.Dot

case class SignedDelta[State](
    dot: Dot,
    signature: Option[Signature],
    payload: State
) {
  def isSignatureValid(using encoder: Encoder[SignedDelta[State]]): Boolean =
    signature.get.verify(PublicIdentity(dot.place.delegate).publicKey, encoder(this.copy(signature = None)))
}

object SignedDelta {
  def fromDelta[State](authorKey: PrivateIdentity, dot: Dot, state: State)(
      using encoder: Encoder[SignedDelta[State]]
  ): SignedDelta[State] = {
    require(authorKey.getPublic.id == dot.place.delegate)
    val delta = SignedDelta(dot, None, state)
    delta.copy(signature = Some(Signature.compute(encoder(delta), authorKey.identityKey.getPrivate)))
  }
}
