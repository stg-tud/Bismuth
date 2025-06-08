package rdts.syntax

import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap}
import ObserveRemoveMap.Entry
import rdts.dotted.{Dotted, Obrem}
import rdts.syntax.DeltaAWLWWMContainer.State
import rdts.time.Dots

/** This is used for the encrypted todolist and associated benchmark */
class DeltaAWLWWMContainer[K, V](
    val replicaId: LocalUid,
    initialState: State[K, V] = DeltaAWLWWMContainer.empty[K, V],
) {
  protected var _state: State[K, V] = initialState

  def state: State[K, V] = _state

  given LocalUid = replicaId

  def get(key: K): Option[V] =
    _state.get(key).map(_.value.value)

  def put(key: K, value: V): Unit = { putDelta(key, value); () }

  def putDelta(key: K, value: V): State[K, V] = {
    val delta = {
      val nextDot = Dots.single(_state.repr.context.nextDot(replicaId.uid))
      _state.transformPlain(key) {
        case Some(prior) => Some(Entry(nextDot, prior.value.write(value)))
        case None        => Some(Entry(nextDot, LastWriterWins.now(value)))
      }
    }
    mutate(delta)
    delta
  }

  def remove(key: K): Unit = { removeDelta(key); () }

  def removeDelta(key: K): State[K, V] = {
    val delta = {
      _state.remove(key)
    }
    mutate(delta)
    delta
  }

  def removeAllDelta(keys: Seq[K]): State[K, V] = {
    val delta = _state.clear()
    mutate(delta)
    delta
  }

  def values: Map[K, V] =
    _state.entries.map((k, v) => (k, v.value.value)).toMap

  def merge(other: State[K, V]): Unit = {
    mutate(other)
  }

  private def mutate(delta: State[K, V]): Unit = {
    _state = _state `merge` delta
  }
}

object DeltaAWLWWMContainer {
  type Inner[K, V] = ObserveRemoveMap[K, Entry[LastWriterWins[V]]]
  type State[K, V] = Inner[K, V]

  def empty[K, V]: State[K, V] =
    ObserveRemoveMap.empty[K, Entry[LastWriterWins[V]]]

  given lattice[K, V]: Lattice[State[K, V]] = Lattice.derived

}
