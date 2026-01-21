package lofi_acl.sync.signed

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.HashDag.Encoder
import lofi_acl.bft.{Acl, Signature}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.Dot

case class SignedDelta[State](
    dot: Dot,
    signature: Signature,
    payload: State
) {
  def filtered(permissionTree: PermissionTree)(using Filter[State]): Option[SignedDelta[State]] =
    Option.when(Filter[State].isAllowed(payload, permissionTree))(this)

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
