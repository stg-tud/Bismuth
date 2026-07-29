package test.rdts.protocols.knowledgeGroups
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.protocols.Paxos.given_Ordering_BallotNum_PaxosRound
import rdts.protocols.Util.Agreement
import rdts.protocols.Util.Agreement.Undecided
import rdts.protocols.{Participants, Paxos, PaxosRound, Voting}
import rdts.protocols.knowledgeGroups.{KnowledgeGroup, MultiPaxos, PrdtSystem}

import scala.math.Ordering.comparatorToOrdering

class PaperGroups extends munit.FunSuite {
  val client    = Uid.gen()
  val leader    = Uid.gen()
  val follower1 = Uid.gen()
  val follower2 = Uid.gen()
  val proxy     = Uid.gen()

  def clientServerSystem[A] = PrdtSystem[MultiPaxos[A]](Set(
    KnowledgeGroup( // leader and followers share slots
      ids = Set(leader, follower1, follower2),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = _ => true
    ),
    KnowledgeGroup( // client and leader share requests
      ids = Set(client, leader),
      path = m => MultiPaxos[A](requests = m.requests),
      enabled = _ => true
    ),
    KnowledgeGroup( // client and leader share log
      ids = Set(client, leader),
      path = m => MultiPaxos[A](log = m.log),
      enabled = _ => true
    )
  ))

  def occamsRazorSystem[A] = PrdtSystem[MultiPaxos[A]](Set(
    KnowledgeGroup( // leader and followers share slots
      ids = Set(leader, follower1, follower2),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = _ => true
    ),
    KnowledgeGroup( // client and leader share requests
      ids = Set(client, leader),
      path = m => MultiPaxos[A](requests = m.requests),
      enabled = _ => true
    ),
    KnowledgeGroup( // everybody shares decisions
      ids = Set(client, leader, follower1, follower2),
      path = m => MultiPaxos[A](log = m.log),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          paxos.decision(using Participants(Set(leader, follower1, follower2))) != Agreement.Undecided
        )
    )
  ))

  def compartmentalizedSystem[A] = PrdtSystem[MultiPaxos[A]](Set(
    KnowledgeGroup( // leader proxy is phase2a or leaderElection
      ids = Set(leader, proxy),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          paxos.currentRound.flatMap(_.proposals.votes.headOption.map(_.voter == leader)).getOrElse(false) ||
            paxos.currentRound.flatMap(_.leaderElection.votes.headOption.map(_.voter == leader)).getOrElse(false)
        )
    ),
    KnowledgeGroup( // leader proxy is decided
      ids = Set(leader, proxy),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          paxos.decision(using Participants(Set(leader, follower1, follower2))) != Agreement.Undecided
        )
    ),
    KnowledgeGroup( // proxy followers
      ids = Set(proxy, follower1, follower2),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = _ => true
    ),
    KnowledgeGroup( // client leader requests
      ids = Set(client, leader),
      path = m => MultiPaxos[A](requests = m.requests),
      enabled = _ => true
    ),
    KnowledgeGroup( // client leader log
      ids = Set(client, leader),
      path = m => MultiPaxos[A](log = m.log),
      enabled = _ => true
    ),
  ))

  test("ClientServerSystem Smoke Test") {
    val a = MultiPaxos[Int]()
    given Participants(Set(leader, follower1, follower2))
    given LocalUid(leader)
    val delta = a.startLeaderElection(0)

    assert(clientServerSystem.matches(delta, follower1))
    assert(clientServerSystem.matches(delta, leader))
  }

  test("Compartmentalization") {
    var paxosLeader    = MultiPaxos[Int]()
    var paxosFollower1 = MultiPaxos[Int]()
    var paxosFollower2 = MultiPaxos[Int]()
    var paxosProxy = MultiPaxos[Int]()
    var proxyDeltaBuffer = MultiPaxos[Int]()
    given Participants(Set(leader, follower1, follower2))

    val d1 = {
      given LocalUid(leader)

      //// start leader election
      val delta = paxosLeader.startLeaderElection(0)
      // should match only proxy
      assert(compartmentalizedSystem.matches(delta, proxy))
      assert(compartmentalizedSystem.matches(delta, leader))
      assert(!compartmentalizedSystem.matches(delta, follower1))
      assert(!compartmentalizedSystem.matches(delta, client))

      paxosLeader = paxosLeader.merge(delta)
      paxosLeader = paxosLeader.merge(paxosLeader.upkeep)
      delta
    }
    {
      // delta from before can be forwarded by proxy
      given LocalUid(proxy)
      paxosProxy = paxosProxy.merge(d1)

      // should match only leader and followers
      assert(compartmentalizedSystem.matches(d1, proxy))
      assert(compartmentalizedSystem.matches(d1, leader))
      assert(compartmentalizedSystem.matches(d1, follower1))
      assert(compartmentalizedSystem.matches(d1, follower1))
      assert(!compartmentalizedSystem.matches(d1, client))
    }
    val d2 = {
      // delta from before is received by follower1
      given LocalUid(follower1)
      paxosFollower1 = paxosFollower1.merge(d1)
      // perform upkeep -> vote in leader election
      val delta = paxosFollower1.upkeep
      paxosFollower1 = paxosFollower1.merge(delta)

      // upkeep delta should match only follower2 and proxy
      assert(compartmentalizedSystem.matches(delta, proxy))
      assert(!compartmentalizedSystem.matches(delta, leader))
      assert(compartmentalizedSystem.matches(delta, follower1))
      assert(compartmentalizedSystem.matches(delta, follower1))
      assert(!compartmentalizedSystem.matches(delta, client))
      delta
    }
    {
      // delta from before can be forwarded by proxy
      given LocalUid(proxy)
      paxosProxy = paxosProxy.merge(d2)

      // should match only leader and followers
      assert(compartmentalizedSystem.matches(d2, proxy))
      assert(!compartmentalizedSystem.matches(d2, leader))
      assert(compartmentalizedSystem.matches(d2, follower1))
      assert(compartmentalizedSystem.matches(d2, follower1))
      assert(!compartmentalizedSystem.matches(d2, client))
    }
    val d3 = {
      // leader receives leader election delta and starts proposal
      given LocalUid(leader)
      paxosLeader = paxosLeader.merge(d2)

      //// start leader election
      val delta = paxosLeader.proposeIfLeader(0, 0)
      // should match only proxy
      assert(compartmentalizedSystem.matches(delta, proxy))
      assert(compartmentalizedSystem.matches(delta, leader))
      assert(!compartmentalizedSystem.matches(delta, follower1))
      assert(!compartmentalizedSystem.matches(delta, client))

      paxosLeader = paxosLeader.merge(delta)
      paxosLeader = paxosLeader.merge(paxosLeader.upkeep)
      delta
    }
    {
      // delta from before can be forwarded by proxy
      given LocalUid(proxy)
      paxosProxy = paxosProxy.merge(d3)
      proxyDeltaBuffer = proxyDeltaBuffer.merge(d3)

      // should match only leader and followers
      assert(compartmentalizedSystem.matches(d3, proxy))
      assert(compartmentalizedSystem.matches(d3, leader))
      assert(compartmentalizedSystem.matches(d3, follower1))
      assert(compartmentalizedSystem.matches(d3, follower2))
      assert(!compartmentalizedSystem.matches(d3, client))
    }
    val d4 = {
      // delta from before is received by follower1
      given LocalUid(follower1)
      paxosFollower1 = paxosFollower1.merge(d3)
      // perform upkeep -> accept proposal
      val delta = paxosFollower1.upkeep
      paxosFollower1 = paxosFollower1.merge(delta)

      // upkeep delta should match only follower2 and proxy
      assert(compartmentalizedSystem.matches(delta, proxy))
      assert(!compartmentalizedSystem.matches(delta, leader))
      assert(compartmentalizedSystem.matches(delta, follower1))
      assert(compartmentalizedSystem.matches(delta, follower2))
      assert(!compartmentalizedSystem.matches(delta, client))
      delta
    }
    {
      // delta from before cannot yet be forwarded by proxy because it does not contain the decision
      given LocalUid(proxy)
      paxosProxy = paxosProxy.merge(d4)
      proxyDeltaBuffer = proxyDeltaBuffer.merge(d4)

      val proxyGroup =
        KnowledgeGroup[MultiPaxos[Int]]( // leader proxy is decided
          ids = Set(leader, proxy),
          path = m => MultiPaxos[Int](slots = m.slots),
          enabled = m =>
            m.decision(using Participants(Set(leader, follower1, follower2))) != Agreement.Undecided
        )
      // should match only proxy and followers
      assert(compartmentalizedSystem.matches(d4, proxy))
      assert(!compartmentalizedSystem.matches(d4, leader))
      assert(compartmentalizedSystem.matches(d4, follower1))
      assert(compartmentalizedSystem.matches(d4, follower2))
      assert(!compartmentalizedSystem.matches(d4, client))

      // however, we can construct a delta that contains the decision
      val currentSlot = paxosProxy.slots.size.toLong - 1
      val paxosDelta = {
        val currRound = paxosProxy.slots(currentSlot).rounds.maxOption
        currRound.map((ballot, round) => Paxos(Map(ballot -> PaxosRound(proposals = round.proposals))))
      }.getOrElse(Paxos())
      val decDelta = MultiPaxos(slots = Map(currentSlot -> paxosDelta))

      assert(!Bottom[MultiPaxos[Int]].isEmpty(decDelta))
      assert(compartmentalizedSystem.matches(decDelta, leader))
      assert(paxosProxy.subsumes(decDelta)) // this is safe because we only repackage existing knowledge

      // alternatively, we can use the buffered deltas from the follower
      assert(compartmentalizedSystem.matches(proxyDeltaBuffer, leader))
      assert(paxosProxy.subsumes(proxyDeltaBuffer)) // this is safe because we only repackage existing knowledge

    }
  }

}
