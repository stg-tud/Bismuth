package lofi_acl.collections

import lofi_acl.collections.DeltaAWLWWMContainer.{Entry, State}
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap}
import rdts.dotted.{HasDots, Obrem}
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

  def removeAllDelta(): State[K, V] = {
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

  case class Entry[V](dots: Dots, value: V)
  object Entry {
    given bottom[V: Bottom]: Bottom[Entry[V]]    = Bottom.derived
    given lattice[V: Lattice]: Lattice[Entry[V]] = Lattice.derived

    given hasDots[V]: HasDots[Entry[V]] = new HasDots[Entry[V]] {
      extension (dotted: Entry[V]) {
        override def dots: Dots = dotted.dots

        override def removeDots(dots: Dots): Option[Entry[V]] =
          val res = dotted.dots `subtract` dots
          Option.when(!res.isEmpty):
            Entry(res, dotted.value)
      }
    }
  }

  type State[K, V] = ObserveRemoveMap[K, Entry[LastWriterWins[V]]]

  def empty[K, V]: State[K, V] =
    ObserveRemoveMap.empty[K, Entry[LastWriterWins[V]]]

  given lattice[K, V]: Lattice[State[K, V]] = Lattice.derived

}
