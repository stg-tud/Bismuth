package ex2025recipebook

import rdts.base.Historized.MetaDelta
import rdts.base.{Historized, Lattice}
import rdts.time.Dots

abstract class DeltaBuffer[A, Self <: DeltaBuffer[A, Self]](buffer: Set[MetaDelta[A]]) {

  def applyDelta(delta: MetaDelta[A]): Self

  def applyDelta(delta: A, id: Dots): Self

  def clearDeltas(): Self

  def getDeltas(seen: Dots): Set[MetaDelta[A]] = buffer.filterNot(d => seen.contains(d.id))

  inline def getSize: Int = buffer.size

}

/** A delta buffer */
class DeltaBufferEverything[A](buffer: Set[MetaDelta[A]]) extends DeltaBuffer[A, DeltaBufferEverything[A]](buffer) {

  def applyDelta(metaDelta: MetaDelta[A]): DeltaBufferEverything[A] =
    DeltaBufferEverything(buffer + metaDelta)

  def applyDelta(delta: A, id: Dots): DeltaBufferEverything[A] = {
    val newMetaDelta = MetaDelta(id, delta)
    DeltaBufferEverything(buffer + newMetaDelta)
  }

  def clearDeltas(): DeltaBufferEverything[A] = DeltaBufferEverything(Set.empty[MetaDelta[A]])

}

object DeltaBufferEverything {
  def apply[A](set: Set[MetaDelta[A]]): DeltaBufferEverything[A] = new DeltaBufferEverything(set)
  def apply[A](): DeltaBufferEverything[A]                       = new DeltaBufferEverything(Set.empty[MetaDelta[A]])
}

/** A delta buffer with test for redundancy */
class DeltaBufferNonRedundant[A: Historized](buffer: Set[MetaDelta[A]], redundantDots: Dots)
    extends DeltaBuffer[A, DeltaBufferNonRedundant[A]](buffer) {

  def applyDelta(metaDelta: MetaDelta[A]): DeltaBufferNonRedundant[A] = {
    val redundantDeltas = buffer.getRedundantDeltas(metaDelta.delta)
    val newMetaDelta = metaDelta.copy(redundantDots = metaDelta.redundantDots.union(redundantDeltas))
    val newRedundantDots = redundantDeltas.union(redundantDots)

    DeltaBufferNonRedundant(buffer.filterNot(metaDelta => newRedundantDots.contains(metaDelta.id)) + newMetaDelta, redundantDots)
  }

  def applyDelta(delta: A, id: Dots): DeltaBufferNonRedundant[A] = {
    val redundantDeltas  = buffer.getRedundantDeltas(delta)
    val newMetaDelta     = MetaDelta(id, delta, redundantDeltas)
    val newRedundantDots = redundantDeltas.union(redundantDots)

    DeltaBufferNonRedundant(
      buffer.filterNot(metaDelta => newRedundantDots.contains(metaDelta.id)) + newMetaDelta,
      newRedundantDots
    )
  }

  def clearDeltas(): DeltaBufferNonRedundant[A] = DeltaBufferNonRedundant(Set.empty[MetaDelta[A]], Dots.empty)

}

object DeltaBufferNonRedundant {
  def apply[A: Historized](set: Set[MetaDelta[A]], dots: Dots): DeltaBufferNonRedundant[A] =
    new DeltaBufferNonRedundant(set, dots)
  def apply[A: Historized](): DeltaBufferNonRedundant[A] =
    new DeltaBufferNonRedundant(Set.empty[MetaDelta[A]], Dots.empty)
}

/** A delta buffer with test for subsumption */
class DeltaBufferSubsumed[A: Lattice](buffer: Set[MetaDelta[A]])
    extends DeltaBuffer[A, DeltaBufferSubsumed[A]](buffer) {

  def applyDelta(metaDelta: MetaDelta[A]): DeltaBufferSubsumed[A] = {
    DeltaBufferSubsumed(buffer.filterNot(bufferedDelta => metaDelta.delta `subsumes` bufferedDelta.delta) + metaDelta)
  }

  def applyDelta(delta: A, id: Dots): DeltaBufferSubsumed[A] = {
    val newMetaDelta = MetaDelta(id, delta)
    DeltaBufferSubsumed(buffer.filterNot(bufferedDelta => delta `subsumes` bufferedDelta.delta) + newMetaDelta)
  }

  def clearDeltas(): DeltaBufferSubsumed[A] = DeltaBufferSubsumed(Set.empty[MetaDelta[A]])

}

object DeltaBufferSubsumed {
  def apply[A: Lattice](set: Set[MetaDelta[A]]): DeltaBufferSubsumed[A] = new DeltaBufferSubsumed(set)
  def apply[A: Lattice](): DeltaBufferSubsumed[A] = new DeltaBufferSubsumed(Set.empty[MetaDelta[A]])
}
