package rdts.protocols.chimeric

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.protocols.Util.Agreement
import rdts.protocols.Util.Agreement.*
import rdts.protocols.{BallotNum, Consensus, Participants, PaxosRound, Voting}


/** Replicated consensus state for one network and its Paxos rounds. */
case class ChimericOpen[A](
    network: OpenNetwork,
    rounds: Map[(ConfigId, BallotNum), PaxosRound[A]] = Map.empty
):

  import ChimericOpen.given

  /** ID of the currently active network configuration. */
  private def activeConfigId: ConfigId =
    network.currentConfigId

  /** Newest round across all known configurations. */
  def currentRound: Option[((ConfigId, BallotNum), PaxosRound[A])] =
    rounds.maxByOption(_._1)(using summon[Ordering[(ConfigId, BallotNum)]])

  /** Newest round belonging to a configuration. */
  def currentRoundIn(cfgId: ConfigId): Option[(BallotNum, PaxosRound[A])] =
    rounds.iterator
      .collect { case ((cid, b), r) if cid == cfgId => (b, r) }
      .toList
      .maxByOption(_._1)(using summon[Ordering[BallotNum]])

  /** Newest round in the active configuration. */
  def activeRound: Option[(BallotNum, PaxosRound[A])] =
    currentRoundIn(activeConfigId)

  /** Ballot number of the active round. */
  def currentBallotNum: BallotNum =
    activeRound.map(_._1).get

  /** Leader-election votes in the active round. */
  def currentLeaderElection: Option[Voting[Uid]] =
    activeRound.map(_._2.leaderElection)

  /** Candidate selected by the active leader-election round. */
  def leaderCandidate: Uid =
    currentLeaderElection
      .flatMap(_.votes.headOption)
      .map(_.value)
      .get

  /** Newest round in a configuration that contains a value vote. */
  def lastValueVoteIn(cfgId: ConfigId): Option[(BallotNum, PaxosRound[A])] =
    rounds.iterator
      .collect {
        case ((cid, b), r)
            if cid == cfgId && r.proposals.votes.nonEmpty =>
          (b, r)
      }
      .toList
      .maxByOption(_._1)(using summon[Ordering[BallotNum]])

  /** Newest value-voting round in the active configuration. */
  def lastValueVote: Option[((ConfigId, BallotNum), PaxosRound[A])] =
    lastValueVoteIn(activeConfigId)
      .map { case (b, r) => ((activeConfigId, b), r) }

  /** Value from the newest received proposal, if one exists. */
  def newestReceivedVal: Option[A] =
    lastValueVoteIn(activeConfigId)
      .flatMap(_._2.proposals.votes.headOption)
      .map(_.value)

  /** Value proposed locally in the active configuration. */
  def myValue(using LocalUid): Option[A] =
    rounds
      .get((activeConfigId, BallotNum(replicaId, -1)))
      .flatMap(_.proposals.votes.headOption)
      .map(_.value)

  /** Next ballot number for the local replica. */
  def nextBallotNum(using LocalUid): BallotNum =
    val maxCounter =
      rounds.keys
        .collect {
          case (cid, b) if cid == activeConfigId =>
            b.counter
        }
        .maxOption
        .getOrElse(-1L)

    BallotNum(replicaId, maxCounter + 1)

  /** Add a leader vote to the active round. */
  def voteLeader(leader: Uid)(using LocalUid): PaxosRound[A] =
    PaxosRound(
      leaderElection =
        activeRound
          .map(_._2.leaderElection)
          .getOrElse(Voting[Uid]())
          .voteFor(leader),
      proposals =
        activeRound
          .map(_._2.proposals)
          .getOrElse(Voting[A]())
    )

  /** Add a value vote to the active round. */
  def voteValue(value: A)(using LocalUid): PaxosRound[A] =
    PaxosRound(
      leaderElection =
        activeRound
          .map(_._2.leaderElection)
          .getOrElse(Voting[Uid]()),
      proposals =
        activeRound
          .map(_._2.proposals)
          .getOrElse(Voting[A]())
          .voteFor(value)
    )

  /** Whether the active round contains a leader candidate. */
  def currentRoundHasCandidate: Boolean =
    activeRound.exists {
      case (_, PaxosRound(leaderElection, _)) =>
        leaderElection.votes.nonEmpty
    }

  /** Whether the active round contains a value proposal. */
  def currentRoundHasProposal: Boolean =
    activeRound.exists {
      case (_, PaxosRound(_, proposals)) =>
        proposals.votes.nonEmpty
    }

  /** Whether the local replica is the elected leader. */
  def isCurrentLeader(using LocalUid): Boolean =
    activeRound match
      case Some((_, PaxosRound(leaderElection, _))) =>
        leaderDecision(leaderElection)(
          using network.currentConfig.slices
        ) match
          case Decided(l) => l == replicaId
          case _          => false
      case None =>
        false

  /** Start a leader-election round and record the local value. */
  def phase1a(value: A)(using LocalUid): ChimericOpen[A] =
    copy(
      rounds = rounds ++ Map(
        (activeConfigId, nextBallotNum) ->
          voteLeader(replicaId),
        (activeConfigId, BallotNum(replicaId, -1)) ->
          PaxosRound(
            proposals = Voting[A]().voteFor(value)
          )
        )
      )

  /** Start a leader-election round without proposing a value. */
  def phase1a(using LocalUid): ChimericOpen[A] =
    copy(
      rounds = rounds ++ Map(
        (activeConfigId, nextBallotNum) ->
          voteLeader(replicaId)
      )
    )

  /** Respond to the current leader-election proposal. */
  def phase1b(using LocalUid): ChimericOpen[A] =
    if !currentRoundHasCandidate then
      this
    else
      lastValueVoteIn(activeConfigId) match
        case Some((promisedBallot, acceptedVal)) =>
          copy(
            rounds = rounds ++ Map(
              (activeConfigId, currentBallotNum) ->
                voteLeader(leaderCandidate),
              (activeConfigId, promisedBallot) ->
                acceptedVal
            )
          )

        case None =>
          copy(
            rounds = rounds ++ Map(
              (activeConfigId, currentBallotNum) ->
                voteLeader(leaderCandidate)
            )
          )

  /** As leader, propose the newest accepted value or `myProposal`. */
  def phase2a(myProposal: A)(using LocalUid): ChimericOpen[A] =
    if !isCurrentLeader then
      this
    else
      newestReceivedVal match
        case Some(v) =>
          copy(
            rounds = rounds ++ Map(
              (activeConfigId, currentBallotNum) ->
                voteValue(v)
            )
          )

        case None =>
          copy(
            rounds = rounds ++ Map(
              (activeConfigId, currentBallotNum) ->
                voteValue(myProposal)
            )
          )

  /** Propose the value previously recorded by this replica. */
  def phase2a(using LocalUid): ChimericOpen[A] =
    myValue match
      case Some(v) => phase2a(v)
      case None    => this

  /** Accept the value currently proposed in the active round. */
  def phase2b(using LocalUid): ChimericOpen[A] =
    if !currentRoundHasProposal then
      this
    else
      val proposal =
        activeRound.get._2.proposals.votes.head.value

      copy(
        rounds = rounds ++ Map(
          (activeConfigId, currentBallotNum) ->
            voteValue(proposal)
        )
      )

  /** Propose and vote for a network reconfiguration. */
  def proposeReconfiguration(
      op: ReconfigOp
  )(using LocalUid): ChimericOpen[A] =
    val nextCfg =
      op match
        case AddNode(nextId, node, nodeSlices, updatedExistingSlices) =>
          network.deriveConfigWithAddedNode(
            nextId,
            node,
            nodeSlices,
            updatedExistingSlices
          )

        case RemoveNode(nextId, node, replacementSlices) =>
          network.deriveConfigWithoutNode(
            nextId,
            node,
            replacementSlices
          )

        case UpdateSlices(nextId, updatedSlices) =>
          network.deriveConfigWithUpdatedSlices(
            nextId,
            updatedSlices
          )

    val transition =
      network.proposeTransition(nextCfg)

    val updated =
      network
        .knowTransition(transition)
        .voteTransition(transition.from, transition.to)

    copy(network = updated)

  /** Return the configuration transition that has reached quorum, if any. */
  def reconfigurationDecision: Option[ConfigId] =
    network.transitionDecision(network.currentConfigId)

  /** Enact a decided reconfiguration. */
  def enactReconfiguration: ChimericOpen[A] =
    reconfigurationDecision match
      case None =>
        this

      case Some(nextId) =>
        copy(network = network.enact(nextId))

  /** Determine whether the active configuration has reached consensus. */
  def decision: Agreement[A] =
    rounds.toList
      .collect {
        case ((cfgId, b), r)
            if cfgId == network.currentConfigId =>
          ((cfgId, b), r)
      }
      .sortBy(_._1)(using summon[Ordering[(ConfigId, BallotNum)]])
      .reverse
      .iterator
      .map {
        case ((cfgId, _), PaxosRound(_, proposals)) =>
          proposalDecision(proposals)(
            using network.config(cfgId).slices
          )
      }
      .collectFirst {
        case d @ Decided(_) => d
        case Invalid         => Invalid
      }
      .getOrElse(Undecided)

  /** Decide a leader when its votes reach quorum. */
  private def leaderDecision(
      leaderElection: Voting[Uid]
  )(using QuorumConfig): Agreement[Uid] =
    leaderElection.votes
      .groupBy(_.value)
      .iterator
      .collectFirst {
        case (candidate, votes)
            if FBASOpen.isQuorumReached(
              summon[QuorumConfig],
              votes.map(_.voter).toSet
            ) =>
          Decided(candidate)
      }
      .getOrElse(Undecided)

  /** Decide a proposal value when its votes reach quorum. */
  private def proposalDecision(
      proposals: Voting[A]
  )(using QuorumConfig): Agreement[A] =
    proposals.votes
      .groupBy(_.value)
      .iterator
      .collectFirst {
        case (value, votes)
            if FBASOpen.isQuorumReached(
              summon[QuorumConfig],
              votes.map(_.voter).toSet
            ) =>
          Decided(value)
      }
      .getOrElse(Undecided)


object ChimericOpen:

  /** Orders ballots first by counter and then by replica ID. */
  given Ordering[BallotNum] with
    override def compare(x: BallotNum, y: BallotNum): Int =
      if x.counter > y.counter then
        1
      else if x.counter < y.counter then
        -1
      else
        Ordering[Uid].compare(x.uid, y.uid)

  /** Orders configuration/ballot pairs by configuration, then ballot. */
  given Ordering[(ConfigId, BallotNum)] with
    override def compare(
        x: (ConfigId, BallotNum),
        y: (ConfigId, BallotNum)
    ): Int =
      val cfgCmp =
        Ordering[Long].compare(x._1, y._1)

      if cfgCmp != 0 then
        cfgCmp
      else
        summon[Ordering[BallotNum]].compare(x._2, y._2)

  /** Derived lattice instances for rounds and replicated state. */
  given [A]: Lattice[PaxosRound[A]] = Lattice.derived
  given [A]: Lattice[ChimericOpen[A]] = Lattice.derived

  /** ChimericOpen requires an explicit network during bootstrap. */
  given [A]: Bottom[ChimericOpen[A]] with
    override def empty: ChimericOpen[A] =
      throw new IllegalStateException(
        "ChimericOpen.empty requires an explicit bootstrap OpenNetwork"
      )

  /** Create an empty consensus state using the initial configuration. */
  def bootstrap[A](
      initial: NetworkConfig
  ): ChimericOpen[A] =
    ChimericOpen[A](
      network = OpenNetwork.bootstrap(initial),
      rounds = Map.empty[(ConfigId, BallotNum), PaxosRound[A]]
    )

  /** Consensus operations for ChimericOpen. */
  given consensus: Consensus[ChimericOpen] with

    extension [A](c: ChimericOpen[A])
      /** Start a consensus proposal. */
      override def propose(
          value: A
      )(using LocalUid, Participants): ChimericOpen[A] =
        c.phase1a(value)

    extension [A](c: ChimericOpen[A])(using Participants)
      /** Return the decided value, if consensus has been reached. */
      override def result: Option[A] =
        c.decision match
          case Invalid    => None
          case Decided(x) => Some(x)
          case Undecided  => None

    extension [A](c: ChimericOpen[A])
      /** Advance the local consensus state by one protocol step. */
      override def upkeep()(
          using LocalUid,
          Participants
      ): ChimericOpen[A] =
        val afterReconfig =
          c.enactReconfiguration

        afterReconfig.activeRound match
          case Some((_, PaxosRound(leaderElection, _)))
              if afterReconfig.leaderDecision(
                leaderElection
              )(using afterReconfig.network.currentConfig.slices) != Undecided =>

            afterReconfig.leaderDecision(
              leaderElection
            )(using afterReconfig.network.currentConfig.slices) match
              case Decided(l) if l == replicaId =>
                afterReconfig.phase2a

              case Decided(_) =>
                afterReconfig.phase2b

              case _ =>
                afterReconfig.phase1b

          case _ =>
            afterReconfig.phase1b

    override def empty[A]: ChimericOpen[A] =
      summon[Bottom[ChimericOpen[A]]].empty

    override def lattice[A]: Lattice[ChimericOpen[A]] =
      summon[Lattice[ChimericOpen[A]]]
