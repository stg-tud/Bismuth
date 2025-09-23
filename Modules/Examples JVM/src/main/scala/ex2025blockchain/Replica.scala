package ex2025blockchain

import rdts.base.{Lattice, LocalUid}
import rdts.syntax.{DeltaBuffer, DeltaBufferContainer}


/**
 * Copied from 'package ex2025protocols.dare'
 */


/**
 * For this lecuter/exercise, we are going to explore an executable model of a distributed system,
 * consisting of many of these replicas. Each replica has its own current state, and a replica ID.
 * There is no actual network involved, and it is just a helper
 * to make exploring possible behaviour of a distributed system possible.
 * It has no value in a real program.
 */
class Replica[A](init: A) {
  val replicaId: LocalUid             = LocalUid.gen()
  val buffer: DeltaBufferContainer[A] = DeltaBuffer(init).mutable

  def mod(f: LocalUid ?=> A => A)(using Lattice[A]): this.type = {
    buffer.mod(f(using replicaId))
    this
  }

  def receive(other: Replica[A])(using Lattice[A]): this.type = {
    buffer.applyDelta(other.buffer.result.state)
    this
  }

  def show[B](select: A => B = identity): Unit = {
    print(s"$replicaId: ")
    pprint.pprintln(select(buffer.result.state))
  }

}

object Replica {
  def quiescence[A: Lattice](replicas: Replica[A]*): Unit =
    replicas.toList match
      case Seq() | Seq(_) => ()
      case Seq(a, rem*)   =>
        rem.foreach(a.receive)
        rem.foreach(r => r.receive(a))

}
