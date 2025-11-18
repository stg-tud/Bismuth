import rdts.base.{Lattice, LocalUid, Uid}
import rdts.protocols.MultiPaxos
import rdts.protocols.Participants

import scala.collection.View

opaque type ParticipantSet = Set[Uid]

trait StateMachine[A] {
//  val participants: ParticipantSet // Should be set of state machines? or just hide this here?
//  def propose(command: A): StateMachine[A]
  def log: List[A]            // stores the series of commands
  def length: Long // returns the length of the log
  def maxIndex: Long // returns the index of the newest entry
  def get(index: Long): A // returns a specific position in the log
  def upkeep: StateMachine[A] // performs a step that may commit a command
//  def commitIndex: Long
}

case class NonRepStateMachine[A](state: Map[Long,A], index: Long = -1) extends StateMachine[A] {
  override def log: List[A]            = state.toList.sortBy(_._1).map(_._2)
  override def upkeep: StateMachine[A] = this

  override def get(index: Long): A = state(index)

  override def length: Long = index

  override def maxIndex: Long = index

  def append(command: A): StateMachine[A] = NonRepStateMachine[A](state + (index -> command), index + 1)
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
    val steps: View[((Uid, Long), MultiPaxos[A])] = updatedParticipants.map{
      case (uid, stateMachine) if stateMachine.maxIndex > commitIndex(uid) =>
        val lastCommand = stateMachine.get(maxIndex)
        val newIndex = stateMachine.maxIndex
        val delta: MultiPaxos[A] = lastCommand match {
          case MultiPaxosCommands.propose(value) =>  multiPaxos.proposeIfLeader(value)(using LocalUid(uid))
          case MultiPaxosCommands.startLeaderElection => multiPaxos.startLeaderElection(using LocalUid(uid))
        }
        ((uid, newIndex), delta)
      case (uid, _) => ((uid, commitIndex(uid)), MultiPaxos())
    }

    val newPaxos = steps.map(_._2).foldLeft(multiPaxos)((acc, delta) => acc.merge(delta))

    val commitIndexes = steps.map(_._1).toMap

    MultiPaxosStateMachine(multiPaxos = newPaxos, commitIndex = commitIndexes, participants = participants)
  }

  override def length: Long = multiPaxos.log.size

  override def get(index: Long): A = multiPaxos.log(index)

  override def maxIndex: Long = multiPaxos.log.keySet.maxOption.getOrElse(-1)
}

/*
object StateMachine {
  given [A]: Lattice[StateMachine[A]] = {
    given Lattice[ParticipantSet] = Lattice.assertEquals

    Lattice.derived
  }
}
 */
