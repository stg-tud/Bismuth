package replication.research

import rdts.base.{Bottom, Lattice}

/** vibecoded as part of the hyparview experiments */
object OverlayNetworkProtocol {

  case class DemoState(
      connections: OverlayConnectionDirectory.Directory,
  )
  object DemoState {
    given Bottom[DemoState]  = Bottom.derived
    given Lattice[DemoState] = Lattice.derived

    val empty: DemoState = DemoState(OverlayConnectionDirectory.empty)
  }
}
