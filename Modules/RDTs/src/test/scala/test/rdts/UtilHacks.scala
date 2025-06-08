package test.rdts

import rdts.base.Lattice
import rdts.datatypes.LastWriterWins
import rdts.dotted.Dotted
import rdts.syntax.{DeltaBuffer, DeltaBufferContainer}

object UtilHacks2 {
  extension [V](dottedLww: DeltaBuffer[Dotted[LastWriterWins[V]]]) {
    private def workaround3: LastWriterWins[V] = dottedLww.state.data
    export workaround3.read
    def write(v: V)(using Lattice[Dotted[LastWriterWins[V]]]) = dottedLww.transform(x => Dotted(x.data.write(v)))
  }

  extension [V](dottedLww: DeltaBufferContainer[Dotted[LastWriterWins[V]]]) {
    private def workaround4: LastWriterWins[V] = dottedLww.result.state.data
    export workaround4.read
    def write(v: V)(using Lattice[Dotted[LastWriterWins[V]]]) =
      dottedLww.applyDelta(Dotted(dottedLww.result.state.data.write(v)))
  }

}
