package ex2025recipebook

import rdts.base.Historized.MetaDelta
import rdts.base.{Historized, Lattice}
import rdts.time.Dots

/** A delta buffer with test for subsumption
  * @param state
  * @param deltaBuffer
  * @tparam A
  */
case class DeltaBufferWS[A](state: A, deltaBuffer: List[MetaDelta[A]] = List.empty[MetaDelta[A]]) {

  def applyDelta(metaDelta: MetaDelta[A])(using Lattice[A])(using Historized[A]): DeltaBufferWS[A] = {
    println(f">>> metaDelta: $metaDelta")

    val merged = state `merge` metaDelta.delta
    if merged == state then return this
    val subsumedDots = metaDelta.delta.getRedundantDeltas(deltaBuffer.filterNot(bufferedDelta =>
      redundantDots.contains(bufferedDelta.id)
    ))

    val newMetaDelta = metaDelta.copy(redundantDots = metaDelta.redundantDots.union(subsumedDots))

    val b = DeltaBufferWS(merged, newMetaDelta :: deltaBuffer)

    println(f">>> merged: $merged")
    println(f">>> buffer:\n\t\t${b.deltaBuffer.mkString("\n\t\t")}")
    println(f">>> found redundant: $subsumedDots")
    println(f">>> redundant: ${b.redundantDots}")

    b
  }

  def clearDeltas(): DeltaBufferWS[A] = DeltaBufferWS(state)

  def mutable: DeltaBufferWSContainer[A] = new DeltaBufferWSContainer(this)

  inline def mod(f: A => A, dotsId: Dots)(using Lattice[A])(using Historized[A]): DeltaBufferWS[A] =
    applyDelta(MetaDelta(dotsId, f(state)))

  inline def redundantDots: Dots = deltaBuffer.foldLeft(Dots.empty)((dots, bufferedDelta) =>
    if !dots.contains(bufferedDelta.id) then dots.union(bufferedDelta.redundantDots) else dots
  )

}

class DeltaBufferWSContainer[A](var result: DeltaBufferWS[A]) {

  def applyDelta(metaDelta: MetaDelta[A])(using Lattice[A])(using Historized[A]): Unit =
    result = result.applyDelta(metaDelta)

  inline def mod(f: A => A, dotsId: Dots)(using Lattice[A])(using Historized[A]): this.type = {
    applyDelta(MetaDelta(dotsId, f(result.state)))
    this
  }

}
