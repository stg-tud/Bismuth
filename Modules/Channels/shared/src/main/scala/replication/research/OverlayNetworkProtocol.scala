package replication.research

object OverlayNetworkProtocol {
  type DemoState = OverlayStatusProtocol.Status
  val DemoState: OverlayStatusProtocol.type = OverlayStatusProtocol
}
