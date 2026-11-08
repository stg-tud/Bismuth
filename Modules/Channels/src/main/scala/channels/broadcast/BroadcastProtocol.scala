package channels.broadcast

import channels.connection.Peer

/** Typeclass for broadcast protocol state machines driven by a [[channels.ProtocolIO]].
  *
  * `S` is the (immutable) protocol state. The protocol chooses its own types for
  *  - `Msg`: what is exchanged with peers (the driver only needs a codec),
  *  - `Timer`: the purposes of timers it requests (the driver maps them to delays),
  *  - `Out`: what is handed to the application.
  *
  * Only reactions to the network are part of the typeclass. Local operations (what the application wants to
  * disseminate, and what it gets back, such as an assigned dot) are protocol specific, and are run as plain functions
  * `S => (Step, R)` via [[channels.ProtocolIO.update]].
  */
trait BroadcastProtocol[S, Msg, Timer, Out] {
  def addPeer(state: S, peer: Peer): Step[S, Msg, Timer, Out]
  def removePeer(state: S, peer: Peer): Step[S, Msg, Timer, Out]

  /** `from` has been checked by the driver to be a sender the receiving connection may carry. */
  def handleMessage(state: S, from: Peer, message: Msg): Step[S, Msg, Timer, Out]

  /** A timer requested via [[Effect.StartTimer]] expired. */
  def timerFired(state: S, timer: Timer): Step[S, Msg, Timer, Out]
}

/** Effects requested by a protocol transition. */
enum Effect[+Msg, +Timer, +Out] {
  case Send(peers: List[Peer], message: Msg)

  /** Hand `output` to the application. */
  case Deliver(output: Out)

  /** Request a [[BroadcastProtocol.timerFired]] callback later. */
  case StartTimer(timer: Timer)
}

/** Result of a protocol transition: the next state and the effects to perform. */
final case class Step[S, +Msg, +Timer, +Out](state: S, events: Seq[Effect[Msg, Timer, Out]]) {

  /** Wrap the state, e.g., when the protocol is part of a larger state. */
  def mapState[S2](f: S => S2): Step[S2, Msg, Timer, Out] = Step(f(state), events)
}

/** Timers carry only their purpose; the driver decides what delay each purpose maps to. */
enum BroadcastTimer {

  /** Check whether peers announced knowledge we still lack, and graft it. */
  case Repair
}
