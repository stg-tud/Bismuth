package datatypes

import crypto.Ed25519Util
import dag.HashDAG

case class MultiValueRegister[T](
    hashDAG: HashDAG[T]
):

   def write(value: T): MultiValueRegister[T] =
     MultiValueRegister(hashDAG.addEvent(value))

   def read: Set[Option[T]] =
      val heads = hashDAG.getCurrentHeads
      heads.filter(e => !hashDAG.autohrIsByzantine(e.author)).map(event => event.content)

   def merge(other: MultiValueRegister[T]): MultiValueRegister[T] =
     MultiValueRegister[T](this.hashDAG.merge(other.hashDAG))

object MultiValueRegister:
   def apply[T](): MultiValueRegister[T] =
      val keyPair = Ed25519Util.generateNewKeyPair
      MultiValueRegister(HashDAG(keyPair.getPublic, Some(keyPair.getPrivate)))
