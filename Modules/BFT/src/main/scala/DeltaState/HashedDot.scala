package DeltaState

import crypto.Ed25519Util
import java.security.PublicKey

case class HashedDot (
                       replicaID: PublicKey,
                       counter: Integer,
                       hash: String,
                       signature: Array[Byte]
                     ):
  def verifySignature: Boolean =
    Ed25519Util.checkEd25519Signature(s"$counter$hash".getBytes, signature, replicaID)
