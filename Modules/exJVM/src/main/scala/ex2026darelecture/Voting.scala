package ex2026darelecture

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.protocols.Participants.participants
import rdts.protocols.Util.*
import rdts.protocols.{BallotNum, Participants}

case class Voting[A](votes: Map[Uid, A] = Map.empty[Uid, A]) {
  def threshold(using Participants): Int = participants.size / 2 + 1

  def decision(using Participants): Agreement[A] =
    leadingCount match
        case Some((v, count)) if count >= threshold => Agreement.Decided(v)
        case _                                      => Agreement.Undecided

  def voteFor(v: A)(using LocalUid, Participants): Voting[A] =
    precondition(participants.contains(replicaId) && !votes.contains(replicaId)):
        Voting(Map(replicaId -> v))

  def leadingCount: Option[(A, Int)] =
      val grouped: Map[A, Int] = votes.values.groupBy(identity).map((value, vts) => (value, vts.size))
      grouped.maxByOption((_, size) => size)
}

object Voting {
  given [A] => Bottom[Voting[A]]  = Bottom.provide(Voting())
  given [A] => Lattice[Voting[A]] = {
    given Lattice[A] = Lattice.assertEquals
    Lattice.derived
  }

  def main(args: Array[String]): Unit = {
    val a, b, c = Replica(Voting[String]())

    // Every replica participating in this vote.
    given Participants = Participants(Set(a.replicaId.uid, b.replicaId.uid, c.replicaId.uid))

    a.mod(_.voteFor("cat"))
    b.mod(_.voteFor("dog"))
    c.mod(_.voteFor("cat"))
    a.mod(_.voteFor("dog"))

    Replica.quiescence(a, b, c)

    a.show(_.votes) // Map(uida -> "cat", uidb -> "dog", uidc -> "cat")
    a.show(_.decision)
    b.show(_.decision)
    c.show(_.decision)
  }

}

type LeaderElection = Voting[Uid]

case class PaxosRound[A](
    leaderElection: LeaderElection = Voting(),
    proposals: Voting[A] = Voting()
)

case class Paxos[A](rounds: Map[BallotNum, PaxosRound[A]] = Map.empty[BallotNum, PaxosRound[A]])
