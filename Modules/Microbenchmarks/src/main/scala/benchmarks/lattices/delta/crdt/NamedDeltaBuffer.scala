package benchmarks.lattices.delta.crdt

import rdts.base.{Decompose, Lattice, LocalUid, Uid}
import rdts.dotted.{Dotted, HasDots, Obrem}
import rdts.time.{Dot, Dots}

case class Named[T](replicaId: Uid, anon: T)

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
case class NamedDeltaBuffer[State](
    replicaID: LocalUid,
    state: State,
    deltaBuffer: List[Named[State]] = Nil
) {

  inline def map(f: LocalUid ?=> State => State)(using Lattice[State], Decompose[State]): NamedDeltaBuffer[State] =
    applyDelta(replicaID.uid, f(using replicaID)(state))

  def applyDelta(source: Uid, delta: State)(using Lattice[State], Decompose[State]): NamedDeltaBuffer[State] =
    Lattice.diff(state, delta) match {
      case Some(stateDiff) =>
        val stateMerged = Lattice.merge(state, stateDiff)
        new NamedDeltaBuffer(replicaID, stateMerged, Named(source, stateDiff) :: deltaBuffer)
      case None => this
    }

  def mod(f: State => State)(using Lattice[State], Decompose[State]) = applyDelta(replicaID.uid, f(state))
}
