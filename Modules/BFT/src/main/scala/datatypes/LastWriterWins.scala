package datatypes

import crypto.Ed25519Util
import dag.HashDAG
import scala.util.hashing.MurmurHash3

case class LastWriterWins[T](
    hashDAG: HashDAG[T]
):

    def write(value: T): LastWriterWins[T] =
      LastWriterWins(hashDAG.generateDelta(value))

    def read: Option[T] =
        val heads = hashDAG.getCurrentHeads
        if heads.size == 1 then {
          val head = heads.head
          if !hashDAG.autohrIsByzantine(head.author) then
              head.content
          else
              None
        } else
            heads.toList.sortWith((x, y) => MurmurHash3.stringHash(x.id) > MurmurHash3.stringHash(y.id)).find(e =>
              !hashDAG.autohrIsByzantine(e.author)
            ).map(e => e.content.get)

    def merge(lww: LastWriterWins[T]): LastWriterWins[T] =
      LastWriterWins(hashDAG.merge(lww.hashDAG))

object LastWriterWins:
    def apply[T](): LastWriterWins[T] =
        val keyPair = Ed25519Util.generateNewKeyPair
        LastWriterWins(HashDAG(keyPair.getPublic, Some(keyPair.getPrivate)))
