package datatypes

import crypto.Ed25519Util
import dag.HashDAG

import scala.util.hashing.MurmurHash3

case class LastWriterWins[T] (
                               state: HashDAG[T]
                             ):

  def write(value: T): LastWriterWins[T] =
    LastWriterWins(HashDAG(state.authorKeys).addEvent(value))

  def read: Option[T] =
    val heads = state.getCurrentHeads
    if heads.size == 1 then
      heads.head.content
    else
      heads.toList.sortWith((x, y) => MurmurHash3.stringHash(x.id) > MurmurHash3.stringHash(y.id)).head.content
      
  def merge(lww: LastWriterWins[T]): LastWriterWins[T] =
    LastWriterWins(state.merge(lww.state))
    
  
object LastWriterWins:
  def apply[T](): LastWriterWins[T] =
    LastWriterWins(HashDAG(Ed25519Util.generateNewKeyPair))
