package datatypes

import crypto.Ed25519Util
import dag.HashDAG

case class MultiValueRegister[T](
                              state: HashDAG[T]
                            ):

  def write(value: T): MultiValueRegister[T] =
    MultiValueRegister(state.addEvent(value))

  def read: Set[Option[T]] =
    val heads = state.getCurrentHeads
    heads.map(event => event.content)
    
  def merge(other: MultiValueRegister[T]): MultiValueRegister[T] =
    MultiValueRegister[T](this.state.merge(other.state))


object MultiValueRegister:
  def apply[T](): MultiValueRegister[T] =
    MultiValueRegister(HashDAG(Ed25519Util.generateNewKeyPair))