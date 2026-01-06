package lofi_acl.bft

import crypto.channels.PrivateIdentity
import crypto.{Ed25519Util, PublicIdentity}
import lofi_acl.bft.HashDag.{Delta, Encoder}

class BftSignedDeltaRdt[RDT](private val privateIdentity: PrivateIdentity)(using
    protected val encoder: Encoder[SignedDelta[RDT]],
    protected val reconstructor: StateReconstructor[SignedDelta[RDT], RDT]
) {
  private val publicId   = privateIdentity.getPublic
  private val privateKey = privateIdentity.identityKey.getPrivate

  // Can be overridden to specify additional invariants
  def invariants(hash: Hash, delta: SignedDelta[RDT], prefixHashDag: HashDag[SignedDelta[RDT], RDT]): Boolean =
    delta.parents.isEmpty == (hash == prefixHashDag.root) // Only root has no parents

  def mutate(
      delta: RDT,
      hashDag: HashDag[SignedDelta[RDT], RDT]
  ): HashDag[SignedDelta[RDT], RDT] =
      val emptySigDelta = SignedDelta(null, publicId, delta, hashDag.heads)
      val encodedDelta  = encoder(emptySigDelta)
      val signature     = Signature.unsafeFromArray(Ed25519Util.sign(encodedDelta, privateKey))
      val signedDelta   = emptySigDelta.copy(signature = signature)
      val hash          = HashDag.hash(signedDelta)
      require(invariants(hash, signedDelta, hashDag))
      hashDag.add(signedDelta) match {
        case Left(missing)         => throw IllegalStateException() // heads are never missing from valid hashDag
        case Right(updatedHashDag) => updatedHashDag
      }

  def receive(
      signedDelta: SignedDelta[RDT],
      hashDag: HashDag[SignedDelta[RDT], RDT]
  ): Either[Set[Hash], HashDag[SignedDelta[RDT], RDT]] = {
    val deltaHash = encoder(signedDelta)
    require(isSignatureValid(signedDelta))
    val hash = HashDag.hash(signedDelta)
    require(invariants(hash, signedDelta, hashDag))
    hashDag.add(hash, signedDelta)
  }

  private inline def isSignatureValid(delta: SignedDelta[RDT]): Boolean =
    delta.signature.verify(delta.author.publicKey, encoder(delta.copy(signature = null)))

}

case class SignedDelta[RDT](signature: Signature, author: PublicIdentity, rdt: RDT, parents: Set[Hash])
    extends Delta[RDT]

type StateReconstructor[D <: Delta[RDT], RDT] = (prefix: Set[Hash], hashDag: HashDag[D, RDT]) => RDT
