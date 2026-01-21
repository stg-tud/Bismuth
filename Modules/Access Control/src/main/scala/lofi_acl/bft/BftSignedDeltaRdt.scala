package lofi_acl.bft

import crypto.Ed25519Util
import crypto.channels.PrivateIdentity
import lofi_acl.bft.HashDag.{Delta, Encoder, Hashable}

class BftSignedDeltaRdt[State](private val privateIdentity: PrivateIdentity)(using
    val encoder: Encoder[SignedDelta[State]],
    val reconstructor: StateReconstructor[SignedDelta[State], State]
) {
  private val publicId   = privateIdentity.getPublic
  private val privateKey = privateIdentity.identityKey.getPrivate

  // Can be overridden to specify additional invariants
  def invariants(hash: Hash, delta: SignedDelta[State], prefixHashDag: HashDag[SignedDelta[State], State]): Boolean =
    delta.parents.isEmpty == (hash == prefixHashDag.root) // Only root has no parents

  def mutate(
      delta: State,
      hashDag: HashDag[SignedDelta[State], State]
  ): HashDag[SignedDelta[State], State] =
      val emptySigDelta = SignedDelta(null, publicId, delta, hashDag.heads)
      val encodedDelta  = encoder(emptySigDelta)
      val signature     = Signature.unsafeFromArray(Ed25519Util.sign(encodedDelta, privateKey))
      val signedDelta   = emptySigDelta.copy(signature = signature)

      require(invariants(Hashable[SignedDelta[State]].hash(signedDelta), signedDelta, hashDag))
      hashDag.add(signedDelta) match {
        case Left(missing)         => throw IllegalStateException() // heads are never missing from valid hashDag
        case Right(updatedHashDag) => updatedHashDag
      }

  def receive(
      signedDelta: SignedDelta[State],
      hashDag: HashDag[SignedDelta[State], State]
  ): Either[Set[Hash], HashDag[SignedDelta[State], State]] = {
    val hash = Hashable[SignedDelta[State]].hash(signedDelta)
    receiveWithComputedHash(hash, signedDelta, hashDag)
  }

  def receiveWithComputedHash(
      hash: Hash,
      signedDelta: SignedDelta[State],
      hashDag: HashDag[SignedDelta[State], State]
  ): Either[Set[Hash], HashDag[SignedDelta[State], State]] = {
    hashDag.add(hash, signedDelta) match {
      case missing @ Left(_) =>
        missing
      case updated @ Right(_) =>
        require(isSignatureValid(signedDelta))
        require(invariants(hash, signedDelta, hashDag))
        updated
    }
  }

  private inline def isSignatureValid(delta: SignedDelta[State]): Boolean =
    delta.signature.verify(delta.author.publicKey, encoder(delta.copy(signature = null)))

}

type StateReconstructor[D <: Delta[RDT], RDT] = (prefix: Set[Hash], hashDag: HashDag[D, RDT]) => RDT
