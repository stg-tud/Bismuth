package ex2026accessControl.acl.sync

import channels.crypto.PrivateIdentity
import channels.crypto.PublicIdentity
import channels.crypto.Signature
import rdts.time.Dot
import ex2026accessControl.acl.HashDag.Encoder

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
