package rdts.syntax

import rdts.base.Lattice

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
case class DeltaBuffer[A](
    state: A,
    deltaBuffer: List[A] = Nil
) {

  def applyDelta(delta: A)(using Lattice[A]): DeltaBuffer[A] =
    val merged = state `merge` delta
    DeltaBuffer(merged, if merged == state then deltaBuffer else delta :: deltaBuffer)

  def applyDeltaNonAppend(delta: A)(using Lattice[A]): DeltaBuffer[A] =
    val merged = state `merge` delta
    DeltaBuffer(merged, deltaBuffer)

  def clearDeltas(): DeltaBuffer[A] = DeltaBuffer(state)

  def mutable: DeltaBufferContainer[A] = new DeltaBufferContainer(this)

  def transform(f: A => A)(using Lattice[A]): DeltaBuffer[A] = applyDelta(f(state))

  inline def mod(f: A => A)(using Lattice[A]): DeltaBuffer[A] = {
    applyDelta(f(state))
  }

}

class DeltaBufferContainer[A](var result: DeltaBuffer[A]) {
  def applyDelta(delta: A)(using Lattice[A]): Unit =
    result = result.applyDelta(delta)

  inline def mod(f: A => A)(using Lattice[A]): DeltaBufferContainer[A] = {
    applyDelta(f(result.state))
    this
  }
}
