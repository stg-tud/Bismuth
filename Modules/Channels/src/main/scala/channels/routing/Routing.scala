package channels.broadcast

import channels.broadcast.Routing.Step
import channels.connection.Peer

/** Typeclass for broadcast protocol state machines driven by a [[channels.MessagingIO]].
  *
  * The protocol chooses its own types for
  *  - `State`: the (immutable) protocol state,
  *  - `Msg`: what is exchanged with peers (the driver only needs a codec),
  *  - `Out`: what is handed to the application.
  *
  * Only reactions to the network are part of the typeclass. Local operations (what the application wants to
  * disseminate, and what it gets back, such as an assigned dot) are protocol specific, and are run as plain functions
  * `State => (Step, R)` via [[channels.MessagingIO.update]].
  */
trait Routing {
  type State
  type Msg
  type Out

  def addPeer(state: State, peer: Peer): Step[State, Msg, Out]
  def removePeer(state: State, peer: Peer): Step[State, Msg, Out]

  /** `from` has been checked by the driver to be a sender the receiving connection may carry. */
  def handleMessage(state: State, from: Peer, message: Msg): Step[State, Msg, Out]

  /** A timer requested via [[Effect.StartTimer]] expired. */
  def timerFired(state: State, timer: Routing.Timer): Step[State, Msg, Out]
}

object Routing {

  /** Effects requested by a protocol transition. */
  enum Effect[+Msg, +Out] {
    case Send(peers: List[Peer], message: Msg)

    /** Hand `output` to the application. */
    case Deliver(output: Out)

    /** Request a [[Routing.timerFired]] callback later. */
    case StartTimer(timer: Timer)
  }

  /** Result of a protocol transition: the next state and the effects to perform. */
  final case class Step[S, +Msg, +Out](state: S, events: Seq[Effect[Msg, Out]]) {

    /** Wrap the state, e.g., when the protocol is part of a larger state. */
    def mapState[S2](f: S => S2): Step[S2, Msg, Out] = Step(f(state), events)
  }

  /** Timers carry only their purpose; the driver decides what delay each purpose maps to. */
  enum Timer {

    /** Check whether peers announced knowledge we still lack, and graft it. */
    case Repair
  }
}
