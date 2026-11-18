package channels.routing.authz

import channels.crypto.Commitment.RevealedValue
import channels.crypto.Hash

/** The revealed value of the delta commitment event `event`. */
final case class DeltaValue(event: Hash, value: RevealedValue)
