package replication.overlay

import channels.{Connection, PeerConnectInfo}
import rdts.base.Uid

/** Join/bootstrap logic extracted from HyParView maintenance.
  *
  * This models the paper's join phase separately from the steady-state maintenance protocol.
  * The purpose of join is only to establish an initial set of good active edges and to seed passive
  * knowledge through `ForwardJoin` random walks.
  *
  * Algorithm sketch from the paper:
  *   - `Join(newNode)` at the contact node:
  *     1. remember the newcomer
  *     2. add the newcomer to the active view using the connection on which `Join` arrived
  *     3. send `ForwardJoin(newNode, ARWL, self)` through the current active peers
  *
  *   - `ForwardJoin(newNode, ttl, sender)` at an intermediate node:
  *     1. remember the newcomer
  *     2. if `ttl == PRWL`, insert newcomer into the passive view
  *     3. if `ttl == 0` or there is no onward active neighbor, terminate the walk and insert the newcomer
  *        into the active view
  *     4. otherwise forward the request to a random active peer different from `sender`
  *
  * Important subtlety:
  * when a `ForwardJoin` terminates, the receiver should conceptually add the newcomer to its active view,
  * but it must not attach the current `from` connection as if it were a direct transport to `newNode`.
  * The `from` connection belongs to the previous hop of the random walk, not to the newcomer.
  * In the paper, adding the newcomer to the active view means initiating/accepting neighbor establishment,
  * not reusing the forwarding hop connection as the newcomer connection.
  *
  * This file currently contains only a compilable placeholder API. Concrete state updates are left to the
  * surrounding HyParView implementation.
  */
object HyparviewJoin {

  enum Message:
    case Join(newNode: PeerConnectInfo)
    case ForwardJoin(newNode: PeerConnectInfo, ttl: Int, sender: Uid)

  import Message.*

  /** Minimal placeholder state required by the extracted join logic.
    * Implementors can later replace these stubs with concrete integration into HyParView state.
    */
  trait State {
    def self: PeerConnectInfo
    def activeSize: Int
    def rememberPeer(peer: PeerConnectInfo): State
    def addPassiveIfEligible(peer: PeerConnectInfo): State
  }

  enum Action {
    case SendForwardJoin(connection: Connection, newNode: PeerConnectInfo, ttl: Int, sender: Uid)
    case SendHighPriorityNeighbor(connection: Connection)
    case RememberPeer(peer: PeerConnectInfo)
    case AddPassive(peer: PeerConnectInfo)
    case AddActiveViaJoin(peer: PeerConnectInfo, connection: Connection)
    case AddActiveByInitiatingNeighbor(peer: PeerConnectInfo)
  }

  final case class Config(
      activeRandomWalkLength: Int,
      passiveRandomWalkLength: Int,
  )

  /** Placeholder helper kept explicit so the extracted file compiles before full integration. */
  def rememberPeer(state: State, peer: PeerConnectInfo): State = state.rememberPeer(peer)

  /** Handle one join/bootstrap message.
    *
    * For now this returns placeholder actions describing what the integrated HyParView implementation
    * should do.
    */
  def handle(state: State, config: Config, message: Message, from: Connection): (State, List[Action]) =
    message match
      case Join(newNode) =>
        val next = rememberPeer(state, newNode)
        (
          next,
          List(
            Action.RememberPeer(newNode),
            Action.AddActiveViaJoin(newNode, from)
          )
        )

      case ForwardJoin(newNode, ttl, sender) =>
        val remembered = rememberPeer(state, newNode)
        if newNode.uid == state.self.uid then (remembered, List(Action.RememberPeer(newNode)))
        else if ttl == 0 || remembered.activeSize <= 1 then
            (
              remembered,
              List(
                Action.RememberPeer(newNode),
                Action.AddActiveByInitiatingNeighbor(newNode)
              )
            )
        else {
          val withPassive =
            if ttl == config.passiveRandomWalkLength then remembered.addPassiveIfEligible(newNode)
            else remembered

          val maybePassiveAction =
            Option.when(ttl == config.passiveRandomWalkLength)(Action.AddPassive(newNode)).toList

          (
            withPassive,
            Action.RememberPeer(newNode) :: maybePassiveAction
          )
        }
}
