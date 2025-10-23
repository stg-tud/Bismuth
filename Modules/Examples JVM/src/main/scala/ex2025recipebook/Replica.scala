package ex2025recipebook

import rdts.base.Historized.MetaDelta
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.time.{Dot, Dots}

/** Copied from 'package ex2025protocols.dare' */

/** For this lecuter/exercise, we are going to explore an executable model of a distributed system,
  * consisting of many of these replicas. Each replica has its own current state, and a replica ID.
  * There is no actual network involved, and it is just a helper
  * to make exploring possible behaviour of a distributed system possible.
  * It has no value in a real program.
  */
class Replica[A: Bottom as B]() {
  val replicaId: LocalUid               = LocalUid.gen()
  val buffer: DeltaBufferWSContainer[A] = DeltaBufferWS(B.empty).mutable
  var dots: Dots                        = Dots.empty

  def mod(f: LocalUid ?=> A => A)(using Lattice[A])(using Historized[A]): this.type = {
    val dot = nextDot
    dots = dots.add(dot)
    println(f">>> replica: $replicaId")
    buffer.mod(f(using replicaId), Dots.single(dot))
    this
  }

  def applyDelta(delta: MetaDelta[A])(using Lattice[A]): Unit = {
    dots = dots.union(delta.id.union(delta.redundantDots))
    buffer.applyDelta(delta)
  }

  def receive(other: Replica[A])(using Lattice[A])(using Historized[A]): this.type = {
    other.getDeltas(dots).foreach(applyDelta)
    this
  }

  def getDeltas(seen: Dots): List[MetaDelta[A]] = buffer.result.deltaBuffer.filterNot(d => seen.contains(d.id))

  def show[B](select: A => B = identity): Unit = {
    print(s"$replicaId: ")
    pprint.pprintln(select(buffer.result.state))
  }

  inline def nextDot: Dot = dots.nextDot(using replicaId)

}

object Replica {
  def quiescence[A: {Lattice, Historized}](replicas: Replica[A]*): Unit =
    replicas.toList match
      case Seq() | Seq(_) => ()
      case Seq(a, rem*)   =>
        rem.foreach(a.receive)
        rem.foreach(r => r.receive(a))

}
