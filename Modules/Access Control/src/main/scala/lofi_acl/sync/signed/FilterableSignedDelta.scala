package lofi_acl.sync.signed

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.HashDag.Encoder
import lofi_acl.bft.Signature
import rdts.filters.{Filter, PermissionTree}
import rdts.time.Dot

case class FilterableSignedDelta[State](
    dot: Dot,
    signature: Signature,
    payload: Option[State]
) {
  def filtered(permission: PermissionTree)(using Filter[State]): FilterableSignedDelta[State] = {
    payload match {
      case Some(rdt) => if Filter[State].isAllowed(rdt, permission) then this else this.copy(payload = None)
      case None      => this
    }
  }

  def isSignatureValid(using encoder: Encoder[FilterableSignedDelta[State]]): Boolean =
    signature.verify(PublicIdentity(dot.place.delegate).publicKey, encoder(this.copy(signature = null)))
}

object FilterableSignedDelta {
  def fromDelta[State](authorKey: PrivateIdentity, dot: Dot, state: State)(
      using encoder: Encoder[FilterableSignedDelta[State]]
  ): FilterableSignedDelta[State] = {
    require(authorKey.getPublic.id == dot.place.delegate)
    val delta = FilterableSignedDelta(dot, null, Some(state))
    delta.copy(signature = Signature.compute(encoder(delta), authorKey.identityKey.getPrivate))
  }
}
