package test.rdts.simulatedNetworkTests.tools

import rdts.base.{Decompose, Lattice, LocalUid}

/** BasicCRDTs are Delta CRDTs that use [[IAntiEntropy]] and [[Network]] as Middleware for exchanging deltas between replicas.
  * They cannot actually be used on multiple connected replicas, but are useful for locally testing the behavior of
  * Delta CRDTs.
  *
  * Generated deltas are automatically propagated to the registered [[IAntiEntropy]] instance, but to apply deltas received
  * by the AntiEntropy instance you need to explicitly call processReceivedDeltas on the CRDT.
  */
class AntiEntropyContainer[State](
    protected val antiEntropy: AntiEntropy[State]
) {
  val replicaID: LocalUid = antiEntropy.localUid

  def state: State = antiEntropy.state

  override def toString: String =
    s"AntiEntropy($replicaID, $state)"

  inline def map(f: LocalUid ?=> State => State)(using
      Lattice[State],
      Decompose[State]
  ): AntiEntropyContainer[State] =
    applyDelta(Named(replicaID.uid, f(using replicaID)(state)))

  def applyDelta(delta: Named[State])(using
      Lattice[State],
      Decompose[State]
  ): AntiEntropyContainer[State] =
    delta match {
      case Named(origin, deltaCtx) =>
        Lattice.diff(state, deltaCtx) match {
          case Some(stateDiff) =>
            val stateMerged = Lattice.merge(state, stateDiff)
            antiEntropy.recordChange(Named(origin, stateDiff), stateMerged)
          case None =>
        }
        this
    }

  def processReceivedDeltas()(using
      u: Lattice[State],
      d: Decompose[State]
  ): AntiEntropyContainer[State] =
    antiEntropy.getReceivedDeltas.foldLeft(this) {
      (crdt, delta) => crdt.applyDelta(delta)
    }

  def data: State = state

  def mod(f: LocalUid ?=> State => State)(using Lattice[State], Decompose[State]): AntiEntropyContainer[State] =
    applyDelta(Named(replicaID.uid, f(using replicaID)(state)))
}

object AntiEntropyContainer {

  /** Creates a new PNCounter instance
    *
    * @param antiEntropy AntiEntropy instance used for exchanging deltas with other replicas
    */
  def apply[State](antiEntropy: AntiEntropy[State]): AntiEntropyContainer[State] =
    new AntiEntropyContainer(antiEntropy)
}
