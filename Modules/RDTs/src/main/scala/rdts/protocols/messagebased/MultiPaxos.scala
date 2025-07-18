package rdts.protocols.messagebased

import rdts.base.LocalUid
import rdts.base.LocalUid.replicaId
import Message.{Prepare, Promise}
import rdts.protocols.BallotNum

enum Message:
  case Prepare(ballotNum: BallotNum)
  case Promise(ballotNum: BallotNum)

trait Messaging:
  def broadcast(message: Message): Unit = ???
  def receive: Message => Unit

class MultiPaxos[A](using LocalUid) extends Messaging:
  var currentBallot: BallotNum       = BallotNum(replicaId, -1)
  var promises: Set[Message.Promise] = Set.empty
  var receivedVal: Option[A]         = None

  def phase1a =
    currentBallot = currentBallot.copy(counter = currentBallot.counter + 1)
    broadcast(Prepare(currentBallot))

  def phase1b(ballotNum: BallotNum) =
    broadcast(Promise(ballotNum))

  def phase2a = ???

  def phase2b = ???

  def receive: Message => Unit =
    case Prepare(b) => phase1b(b)
    case Promise(_) => ???
