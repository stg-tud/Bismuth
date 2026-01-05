package rdts.protocols.smr

import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.protocols.{MultiPaxos, Participants}
import rdts.time.Time

import scala.collection.immutable.{Map, NumericRange}
import scala.collection.View

opaque type ParticipantSet = Set[Uid]

// TODO: Problem: wir wollen möglichst generisch sein über die konkreten state machines, wollen aber trotzdem typeclass derivation und dafür brauchen wir konkrete case classes...
trait StateMachine[A] {
  type myType
//  val participants: ParticipantSet // Should be set of state machines? or just hide this here?
//  def propose(command: A): StateMachine[A]
  def log: List[A]            // stores the series of commands
  def maxIndex: Long          // returns the index of the newest entry TODO: I don't think this is needed
  def get(index: Long): A     // returns a specific position in the log
  def upkeep: myType // performs a step that may commit a command
  def commandAfter(index: Long): Option[(Long, A)] // returns the command after the listed index
  def append(entry: A): StateMachine[A]

  def lattice: Lattice[myType]
//  def commitIndex: Long
}

case class NonRepStateMachine[A](state: Map[Long, A] = Map.empty[Long, A], commitIndex: Long = -1) extends StateMachine[A] {
  override type myType = NonRepStateMachine[A]
  override def log: List[A]            = state.toList.sortBy(_._1).map(_._2)
  override def upkeep: StateMachine[A] = this

  override def get(index: Long): A = state(index)

  override def commandAfter(index: Long): Option[(Long, A)] = state.get(index + 1).map(c => (index + 1, c))

  override def maxIndex: Long = commitIndex

  def append(command: A): StateMachine[A] = NonRepStateMachine[A](state + (commitIndex -> command), commitIndex + 1)

  override def lattice: Lattice[NonRepStateMachine[A]] =
      given Lattice[A]    = Lattice.assertEquals
      given Lattice[Long] = Math.max
      Lattice.derived
}

enum MultiPaxosCommands[+A]:
    case propose(value: A)
    case startLeaderElection

case class MultiPaxosStateMachine[A](
//                                      multiPaxos: MultiPaxos[A],
    participants: Map[Uid, StateMachine[MultiPaxos[A]]] = Map.empty[Uid, StateMachine[MultiPaxos[A]]], // multi paxos deltas
    commitIndex: Map[Uid, Long] = Map.empty[Uid, Long]
) extends StateMachine[A] {

  given Participants(participants.keySet)

//  override def propose(command: A)(using LocalUid): StateMachine[A] = MultiPaxosStateMachine(paxos.proposeIfLeader(command), participants)

  private def perParticipant =
    participants.view.mapValues(s => s.log.foldLeft(MultiPaxos.empty[A])((acc, m) => Lattice.merge(acc, m)))

  def multiPaxos: MultiPaxos[A] = perParticipant.values.foldLeft(MultiPaxos.empty[A])((acc, m) => Lattice.merge(acc, m))

  override def log: List[A] = multiPaxos.read

  def multiPaxosUpdate(update: MultiPaxos[A] => MultiPaxos[A])(using localUid: LocalUid): MultiPaxosStateMachine[A] = {
    val delta = update(perParticipant(replicaId))
    MultiPaxosStateMachine(
      participants = Map(replicaId -> participants(replicaId).append(delta))
    )
  }

  override def upkeep: StateMachine[A] = {
    // perform steps for all inner machines
    val updatedParticipants = participants.view.mapValues(_.upkeep)

    MultiPaxosStateMachine(updatedParticipants.toMap)
  }

  override def get(index: Long): A = multiPaxos.log(index)

  override def maxIndex: Long = multiPaxos.log.keySet.maxOption.getOrElse(-1)

  override def commandAfter(index: Long): Option[(Long, A)] = {
    val a: View[(Time, A)] =
      NumericRange(index + 1, multiPaxos.rounds.counter, 1L).view.flatMap(k => multiPaxos.log.get(k).map((k, _)))
    a.minByOption(_._1)
  }

  override type myType = MultiPaxosStateMachine[A]

  override def append(entry: A): StateMachine[A] = ???

  override def lattice: Lattice[MultiPaxosStateMachine[A]] = ???
}

object MultiPaxosStateMachine {
  given [A]: Lattice[MultiPaxosStateMachine[A]] = MultiPaxosStateMachine[A]().lattice
}

/*
object StateMachine {
  given [A]: Lattice[StateMachine[A]] = {
    given Lattice[ParticipantSet] = Lattice.assertEquals

    Lattice.derived
  }
}
 */
