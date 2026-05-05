package ex2021encfxtodo

import ex2021encfxtodo.SyncedTodoListCrdt.{StateType, given}
import ex2021encfxtodo.sync.{ConnectionManager, DataManagerConnectionManager}
import rdts.base.{Decompose, LocalUid}
import rdts.datatypes.ObserveRemoveMap

object ConnectionManagerFactory {

  given decompose[K, V]: Decompose[ObserveRemoveMap[K, V]] = Decompose.atomic

  var impl: (LocalUid, () => StateType, StateType => Unit) => ConnectionManager[StateType] =
    (replicaId, _, handleStateReceived) =>
      // new P2PConnectionManager[StateType](replicaId, queryCrdtState, handleStateReceived)
      DataManagerConnectionManager[StateType](replicaId, handleStateReceived)

  def connectionManager(
      replicaId: LocalUid,
      query: () => StateType,
      stateReceived: StateType => Unit
  ): ConnectionManager[StateType] =
    impl(replicaId, query, stateReceived)
}
