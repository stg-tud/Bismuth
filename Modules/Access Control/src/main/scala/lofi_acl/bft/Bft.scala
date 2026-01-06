package lofi_acl.bft

import crypto.channels.PrivateIdentity
import crypto.{Ed25519Util, PublicIdentity}
import lofi_acl.bft.HashDag.{Encoder, Hash}

class BftSignedDeltaRdt[RDT](private val privateIdentity: PrivateIdentity)(using
    encoder: Encoder[SignedDelta[RDT]],
    reconstructor: StateReconstructor[RDT, SignedDelta[RDT]]
) {
  private val publicId   = privateIdentity.getPublic
  private val privateKey = privateIdentity.identityKey.getPrivate

  // Can be overridden to specify additional invariants
  def invariants(delta: SignedDelta[RDT]): Boolean = true

  def mutate(
      delta: RDT,
      hashDag: HashDag[SignedDelta[RDT]]
  ): HashDag[SignedDelta[RDT]] =
      val emptySigDelta = SignedDelta(null, publicId, delta, hashDag.heads)
      val encodedDelta  = encoder(emptySigDelta)
      val signature     = Signature.unsafeFromArray(Ed25519Util.sign(encodedDelta, privateKey))
      val signedDelta   = emptySigDelta.copy(signature = signature)
      require(invariants(signedDelta))
      hashDag.add(signedDelta) match {
        case Left(missing)         => throw IllegalStateException() // heads are never missing from valid hashDag
        case Right(updatedHashDag) => updatedHashDag
      }

  def receive(
      signedDelta: SignedDelta[RDT],
      hashDag: HashDag[SignedDelta[RDT]]
  ): Either[Set[Hash], HashDag[SignedDelta[RDT]]] = {
    require(isSignatureValid(signedDelta))
    require(invariants(signedDelta))
    hashDag.add(signedDelta)
  }

  private inline def isSignatureValid(delta: SignedDelta[RDT]): Boolean =
    delta.signature.verify(delta.author.publicKey, encoder(delta.copy(signature = null)))

}

case class SignedDelta[RDT](signature: Signature, author: PublicIdentity, delta: RDT, prefix: Set[Hash])

type StateReconstructor[RDT, Delta] = (prefix: Set[Hash], hashDag: HashDag[Delta]) => RDT
