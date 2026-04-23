package ex2026lofi_acl.sync.anti_entropy

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import ex2026lofi_acl.bft.Signature
import ex2026lofi_acl.bft.HashDag.Encoder
import rdts.time.Dot

case class SignedDelta[State](
    dot: Dot,
    signature: Signature,
    payload: State
) {
  def isSignatureValid(using encoder: Encoder[SignedDelta[State]]): Boolean =
    signature.verify(PublicIdentity(dot.place.delegate).publicKey, encoder(this.copy(signature = null)))
}

object SignedDelta {
  def fromDelta[State](authorKey: PrivateIdentity, dot: Dot, state: State)(
      using encoder: Encoder[SignedDelta[State]]
  ): SignedDelta[State] = {
    require(authorKey.getPublic.id == dot.place.delegate)
    val delta = SignedDelta(dot, null, state)
    delta.copy(signature = Signature.compute(encoder(delta), authorKey.identityKey.getPrivate))
  }
}
