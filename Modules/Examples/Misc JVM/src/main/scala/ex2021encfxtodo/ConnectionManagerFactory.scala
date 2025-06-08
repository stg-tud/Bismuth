package ex2021encfxtodo

import ex2021encfxtodo.SyncedTodoListCrdt.{InnerStateType, StateType, given}
import ex2021encfxtodo.sync.{ConnectionManager, DataManagerConnectionManager}
import rdts.base.{Decompose, LocalUid}
import rdts.datatypes.ObserveRemoveMap
import rdts.dotted.HasDots

object ConnectionManagerFactory {

  given hasDots[K, V: HasDots]: HasDots[ObserveRemoveMap[K, V]] = HasDots.noDots
  given decompose[K, V: Decompose]: Decompose[ObserveRemoveMap[K, V]] = Decompose.atomic


  var impl: (LocalUid, () => StateType, StateType => Unit) => ConnectionManager[StateType] =
    (replicaId, queryCrdtState, handleStateReceived) =>
      // new P2PConnectionManager[StateType](replicaId, queryCrdtState, handleStateReceived)
      DataManagerConnectionManager[InnerStateType](replicaId, handleStateReceived)

  def connectionManager(
      replicaId: LocalUid,
      query: () => StateType,
      stateReceived: StateType => Unit
  ): ConnectionManager[StateType] =
    impl(replicaId, query, stateReceived)
}
