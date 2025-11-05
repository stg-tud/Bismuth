package datatypes

import crypto.Ed25519Util
import dag.HashDAG

case class EnableWinsFlag(
    hashDAG: HashDAG[Boolean]
):

    def read: Boolean =
      hashDAG.getCurrentHeads.exists(e => e.id != "0" && e.content.get)

    def enable(value: Boolean): EnableWinsFlag =
      EnableWinsFlag(hashDAG.generateDelta(true))

    def disable(value: Boolean): EnableWinsFlag =
      EnableWinsFlag(hashDAG.generateDelta(false))

    def merge(other: EnableWinsFlag): EnableWinsFlag =
      EnableWinsFlag(hashDAG.merge(other.hashDAG))

object EnableWinsFlag:
    def apply[T](): EnableWinsFlag =
        val keyPair = Ed25519Util.generateNewKeyPair
        EnableWinsFlag(HashDAG(keyPair.getPublic, Some(keyPair.getPrivate)))
