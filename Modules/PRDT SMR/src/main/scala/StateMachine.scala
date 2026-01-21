import rdts.base.{LocalUid, Uid}
import rdts.protocols.MultiPaxos
import rdts.protocols.Participants
import rdts.time.Time

import scala.collection.View
import scala.collection.immutable.NumericRange

opaque type ParticipantSet = Set[Uid]

trait StateMachine[A] {
//  val participants: ParticipantSet // Should be set of state machines? or just hide this here?
//  def propose(command: A): StateMachine[A]
  def log: List[A]            // stores the series of commands
  def maxIndex: Long          // returns the index of the newest entry TODO: I don't think this is needed
  def get(index: Long): A     // returns a specific position in the log
  def upkeep: StateMachine[A] // performs a step that may commit a command
  def commandAfter(index: Long): Option[(Long, A)] // returns the command after the listed index
//  def commitIndex: Long
}

case class NonRepStateMachine[A](state: Map[Long, A], commitIndex: Long = -1) extends StateMachine[A] {
  override def log: List[A]            = state.toList.sortBy(_._1).map(_._2)
  override def upkeep: StateMachine[A] = this

  override def get(index: Long): A = state(index)

  override def commandAfter(index: Long): Option[(Long, A)] = state.get(index + 1).map(c => (index + 1, c))

  override def maxIndex: Long = commitIndex

  def append(command: A): StateMachine[A] = NonRepStateMachine[A](state + (commitIndex -> command), commitIndex + 1)
}

enum MultiPaxosCommands[+A]:
    case propose(value: A)
    case startLeaderElection

case class MultiPaxosStateMachine[A](
    multiPaxos: MultiPaxos[A],
    participants: Map[Uid, StateMachine[MultiPaxosCommands[A]]],
    commitIndex: Map[Uid, Long]
) extends StateMachine[A] {

  given Participants(participants.keySet)

//  override def propose(command: A)(using LocalUid): StateMachine[A] = MultiPaxosStateMachine(paxos.proposeIfLeader(command), participants)

  override def log: List[A] = multiPaxos.read

  override def upkeep: StateMachine[A] = {
    // perform steps for all inner machines
    val updatedParticipants = participants.view.mapValues(_.upkeep)

    // check what this means for paxos and compute deltas and new indices
    val steps: View[((Uid, Long), MultiPaxos[A])] = updatedParticipants.map {
      case (uid, stateMachine) if stateMachine.maxIndex > commitIndex(uid) =>
        val (newIndex, lastCommand) =
          stateMachine.commandAfter(commitIndex(uid)).get // should be safe because maxIndex is higher
        val delta: MultiPaxos[A] = lastCommand match {
          case MultiPaxosCommands.propose(value)      => multiPaxos.proposeIfLeader(value)(using LocalUid(uid))
          case MultiPaxosCommands.startLeaderElection => multiPaxos.startLeaderElection(using LocalUid(uid))
        }
        ((uid, newIndex), delta)
      case (uid, _) => ((uid, commitIndex(uid)), MultiPaxos())
    }

    val newPaxos = steps.map(_._2).foldLeft(multiPaxos)((acc, delta) => acc.merge(delta))

    val commitIndexes = steps.map(_._1).toMap

    MultiPaxosStateMachine(multiPaxos = newPaxos, commitIndex = commitIndexes, participants = participants)
  }

  override def get(index: Long): A = multiPaxos.log(index)

  override def maxIndex: Long = multiPaxos.log.keySet.maxOption.getOrElse(-1)

  override def commandAfter(index: Long): Option[(Long, A)] = {
    val a: View[(Time, A)] =
      NumericRange(index + 1, multiPaxos.rounds.counter, 1L).view.flatMap(k => multiPaxos.log.get(k).map((k, _)))
    a.minByOption(_._1)
  }
}

/*
object StateMachine {
  given [A]: Lattice[StateMachine[A]] = {
    given Lattice[ParticipantSet] = Lattice.assertEquals

    Lattice.derived
  }
}
 */
