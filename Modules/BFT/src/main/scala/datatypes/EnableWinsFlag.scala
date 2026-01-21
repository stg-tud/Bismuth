package datatypes

import crypto.Ed25519Util
import dag.HashDAG

case class EnableWinsFlag(
    hashDAG: HashDAG[Boolean]
) extends Replica[Boolean, EnableWinsFlag]:

    def read: Boolean =
      hashDAG.getCurrentHeads.exists(e => e.id != "0" && e.content.get)

    def enable: EnableWinsFlag =
      EnableWinsFlag(hashDAG.generateDelta(true))

    def disable: EnableWinsFlag =
      EnableWinsFlag(hashDAG.generateDelta(false))

    def merge(other: EnableWinsFlag): EnableWinsFlag =
      EnableWinsFlag(hashDAG.merge(other.hashDAG))

    override def generateDelta(ids: List[String]): EnableWinsFlag = EnableWinsFlag(hashDAG.getDelta(ids))

object EnableWinsFlag:
    def apply[T](): EnableWinsFlag =
        val keyPair = Ed25519Util.generateNewKeyPair
        EnableWinsFlag(HashDAG(keyPair.getPublic, Some(keyPair.getPrivate)))
