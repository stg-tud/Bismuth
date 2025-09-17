package DeltaState

import java.security.PublicKey

case class HashedDot (
                       replicaID: PublicKey,
                       counter: Integer,
                       hash: String,
                       signature: Array[Byte]
                     )
