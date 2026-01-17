package datatypes

import crypto.Ed25519Util
import dag.HashDAG
import scala.util.hashing.MurmurHash3

case class LastWriterWinsRegister[T](
    hashDAG: HashDAG[T]
) extends Replica[T, LastWriterWinsRegister[T]]:

    def write(value: T): LastWriterWinsRegister[T] =
      LastWriterWinsRegister(hashDAG.generateDelta(value))

    def read: Option[T] =
        val heads = hashDAG.getCurrentHeads
        if heads.size == 1 then {
          val head = heads.head
          if head.id != "0" then
              head.content
          else
              None
        } else
            Some(
              heads.toList.sortWith((x, y) => MurmurHash3.stringHash(x.id) > MurmurHash3.stringHash(y.id))
                .head.content.get
            )

    def merge(lww: LastWriterWinsRegister[T]): LastWriterWinsRegister[T] =
      LastWriterWinsRegister(hashDAG.merge(lww.hashDAG))

    override def generateDelta(ids: List[String]): LastWriterWinsRegister[T] =
      LastWriterWinsRegister(hashDAG.getDelta(ids))

object LastWriterWinsRegister:
    def apply[T](): LastWriterWinsRegister[T] =
        val keyPair = Ed25519Util.generateNewKeyPair
        LastWriterWinsRegister(HashDAG(keyPair.getPublic, Some(keyPair.getPrivate)))
