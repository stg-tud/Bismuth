package ex2025recipebook

import rdts.base.Historized.MetaDelta
import rdts.base.{Historized, Lattice}
import rdts.time.Dots

abstract class DeltaBuffer[A, Self <: DeltaBuffer[A, Self]](buffer: List[MetaDelta[A]]) {

  def applyDelta(delta: MetaDelta[A]): Self

  def applyDelta(delta: A, id: Dots): Self

  def clearDeltas(): Self

  def getDeltas(seen: Dots): List[MetaDelta[A]] = buffer.filterNot(d => seen.contains(d.id))

  inline def getSize: Int = buffer.size

}

/** A delta buffer
 */
class DeltaBufferEverything[A](buffer: List[MetaDelta[A]]) extends DeltaBuffer[A, DeltaBufferEverything[A]](buffer) {

  def applyDelta(metaDelta: MetaDelta[A]): DeltaBufferEverything[A] = {
    DeltaBufferEverything(metaDelta :: buffer)
  }

  def applyDelta(delta: A, id: Dots): DeltaBufferEverything[A] = {
    val newMetaDelta = MetaDelta(id, delta)
    DeltaBufferEverything(newMetaDelta :: buffer)
  }

  def clearDeltas(): DeltaBufferEverything[A] = DeltaBufferEverything(List.empty[MetaDelta[A]])

}

/** A delta buffer with test for redundancy
  */
class DeltaBufferNonRedundant[A: Historized](buffer: List[MetaDelta[A]], redundantDots: Dots) extends DeltaBuffer[A, DeltaBufferNonRedundant[A]](buffer) {

  def applyDelta(metaDelta: MetaDelta[A]): DeltaBufferNonRedundant[A] = {
    DeltaBufferNonRedundant(metaDelta :: buffer, redundantDots)
  }

  def applyDelta(delta: A, id: Dots): DeltaBufferNonRedundant[A] = {
    val redundantDeltas = buffer.getRedundantDeltas(delta)
    val newMetaDelta = MetaDelta(id, delta, redundantDeltas)
    val newRedundantDots = redundantDeltas.union(redundantDots)

    // buffer.filterNot() currently makes this approach twice as slow as subsumption, buffer is iterated twice
    DeltaBufferNonRedundant(newMetaDelta :: buffer.filterNot(metaDelta => newRedundantDots.contains(metaDelta.id)), newRedundantDots)
  }

  def clearDeltas(): DeltaBufferNonRedundant[A] = DeltaBufferNonRedundant(List.empty[MetaDelta[A]], Dots.empty)
//
//  inline def redundantDots: Dots = buffer.foldLeft(Dots.empty)((dots, bufferedDelta) =>
//    if !dots.contains(bufferedDelta.id) then dots.union(bufferedDelta.redundantDots) else dots
//  )

}

/** A delta buffer with test for subsumption
 */
class DeltaBufferSubsumed[A: Lattice](buffer: List[MetaDelta[A]]) extends DeltaBuffer[A, DeltaBufferSubsumed[A]](buffer) {

  def applyDelta(metaDelta: MetaDelta[A]): DeltaBufferSubsumed[A] = {
    DeltaBufferSubsumed(metaDelta :: buffer)
  }

  def applyDelta(delta: A, id: Dots): DeltaBufferSubsumed[A] = {
    val newMetaDelta = MetaDelta(id, delta)
    DeltaBufferSubsumed(newMetaDelta :: buffer.filterNot(bufferedDelta => delta `subsumes` bufferedDelta.delta))
  }

  def clearDeltas(): DeltaBufferSubsumed[A] = DeltaBufferSubsumed(List.empty[MetaDelta[A]])

}
