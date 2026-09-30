package rdts.protocols.chimeric

import rdts.base.{Lattice, LocalUid, Uid}

type ConfigId = Long

/** Membership and quorum slices for one network configuration. */
final case class NetworkConfig(
    id: ConfigId,
    members: Set[Uid],
    slices: QuorumConfig
):
  require(members.nonEmpty, s"config $id must have at least one member")
  require(slices.keySet == members, s"config $id must define slices for every member")
  require(
    slices.values.flatten.flatten.toSet.subsetOf(members),
    s"config $id has slices containing unknown members"
  )
  require(
    slices.forall { case (uid, ss) =>
      ss.nonEmpty && ss.forall(slice => slice.nonEmpty && slice.contains(uid))
    },
    s"config $id requires every node to have non-empty self-containing slices"
  )


object NetworkConfig:
  /** Merge equal configurations component-wise; otherwise keep the newer ID. */
  given Lattice[NetworkConfig] with
    override def merge(left: NetworkConfig, right: NetworkConfig): NetworkConfig =
      if left.id == right.id then
        NetworkConfig(
          id = left.id,
          members = Lattice.merge(left.members, right.members),
          slices = Lattice.merge(left.slices, right.slices)
        )
      else if left.id > right.id then left
      else right


/** A proposed transition between two network configurations. */
final case class ConfigTransition(
    from: ConfigId,
    to: ConfigId,
    next: NetworkConfig
)


/** A node's vote for a configuration transition. */
final case class ConfigTransitionVote(
    from: ConfigId,
    to: ConfigId,
    voter: Uid
)

/** Replicated knowledge of configurations, transitions, votes, and enactments. */
final case class OpenNetwork(
    bootstrapConfigId: ConfigId,
    knownConfigs: Map[ConfigId, NetworkConfig],
    knownTransitions: Map[(ConfigId, ConfigId), ConfigTransition],
    transitionVotes: Set[ConfigTransitionVote],
    enactedConfigs: Set[ConfigId]
):
  /** Highest enacted configuration, or the bootstrap configuration initially. */
  def currentConfigId: ConfigId =
    enactedConfigs.maxOption.getOrElse(bootstrapConfigId)

  /** Currently active configuration. */
  def currentConfig: NetworkConfig =
    knownConfigs(currentConfigId)

  /** Look up a known configuration. */
  def config(id: ConfigId): NetworkConfig =
    knownConfigs(id)

  /** Check whether this replica knows a configuration. */
  def knowsConfig(id: ConfigId): Boolean =
    knownConfigs.contains(id)

  /** Check whether this replica knows a transition. */
  def knowsTransition(from: ConfigId, to: ConfigId): Boolean =
    knownTransitions.contains((from, to))

  /** Record a configuration without enacting it. */
  def knowConfig(cfg: NetworkConfig): OpenNetwork =
    copy(knownConfigs = knownConfigs + (cfg.id -> cfg))

  /** Record a validated configuration transition. */
  def knowTransition(t: ConfigTransition): OpenNetwork =
    require(t.next.id == t.to, s"transition target ${t.to} must match next config id ${t.next.id}")
    require(knownConfigs.contains(t.from), s"unknown source config ${t.from}")
    require(t.to > t.from, s"transition target ${t.to} must be greater than source ${t.from}")
    require(
      FBASOpen.isSafeTransition(config(t.from).slices, t.next.slices),
      s"unsafe transition from config ${t.from} to ${t.to}"
    )
    copy(
      knownConfigs = knownConfigs + (t.to -> t.next),
      knownTransitions = knownTransitions + ((t.from, t.to) -> t)
    )

  /** Add the local node's vote for a known transition. */
  def voteTransition(from: ConfigId, to: ConfigId)(using LocalUid): OpenNetwork =
    val voter = summon[LocalUid].uid

    require(
      knownTransitions.contains((from, to)),
      s"unknown transition ($from -> $to)"
    )
    require(
      config(from).members.contains(voter),
      s"$voter is not a member of config $from"
    )

    copy(
      transitionVotes =
        transitionVotes + ConfigTransitionVote(from, to, voter)
    )

  /** Return the voters currently known for a transition. */
  def votersFor(from: ConfigId, to: ConfigId): Set[Uid] =
    transitionVotes.collect {
      case ConfigTransitionVote(`from`, `to`, voter) => voter
    }

  /** Return the first transition from `from` that has reached quorum. */
  def transitionDecision(from: ConfigId): Option[ConfigId] =
    knownTransitions.keys
      .collect { case (`from`, to) => to }
      .toList
      .sorted
      .find { to =>
        FBASOpen.isQuorumReached(
          config(from).slices,
          votersFor(from, to)
        )
      }

  /** Mark a known configuration as enacted. */
  def enact(to: ConfigId): OpenNetwork =
    require(
      knownConfigs.contains(to),
      s"unknown config $to"
    )

    copy(enactedConfigs = enactedConfigs + to)

  /** Derive a configuration that adds a node. */
  def deriveConfigWithAddedNode(
      nextId: ConfigId,
      node: Uid,
      nodeSlices: QuorumSlices,
      updatedExistingSlices: QuorumConfig = Map.empty
  ): NetworkConfig =
    val old = currentConfig

    NetworkConfig(
      id = nextId,
      members = old.members + node,
      slices = old.slices ++ updatedExistingSlices + (node -> nodeSlices)
    )

  /** Derive a configuration that removes a node. */
  def deriveConfigWithoutNode(
      nextId: ConfigId,
      node: Uid,
      replacementSlices: QuorumConfig
  ): NetworkConfig =
    val old = currentConfig
    val nextMembers = old.members - node

    require(
      old.members.contains(node),
      s"node $node not present in current config"
    )
    require(
      replacementSlices.keySet == nextMembers,
      s"replacement slices must define exactly the remaining members"
    )
    require(
      replacementSlices.values.flatten.flatten.toSet.subsetOf(nextMembers),
      s"replacement slices cannot reference removed node $node"
    )
    require(
      replacementSlices.forall { case (uid, slices) =>
        slices.nonEmpty &&
        slices.forall { slice =>
          slice.nonEmpty && slice.contains(uid)
        }
      },
      "replacement slices must be non-empty and self-containing"
    )
    require(
      FBASOpen.hasQuorumIntersection(replacementSlices),
      s"removing $node would break quorum intersection"
    )

    NetworkConfig(
      id = nextId,
      members = nextMembers,
      slices = replacementSlices
    )

  /** Derive a configuration with updated quorum slices. */
  def deriveConfigWithUpdatedSlices(
      nextId: ConfigId,
      updatedSlices: QuorumConfig
  ): NetworkConfig =
    val old = currentConfig

    NetworkConfig(
      id = nextId,
      members = old.members,
      slices = old.slices ++ updatedSlices
    )

  /** Create a transition from the current configuration to `next`. */
  def proposeTransition(next: NetworkConfig): ConfigTransition =
    ConfigTransition(
      from = currentConfigId,
      to = next.id,
      next = next
    )


object OpenNetwork:
  /** Create the initial network state. */
  def bootstrap(initial: NetworkConfig): OpenNetwork =
    require(
      FBASOpen.hasQuorumIntersection(initial.slices),
      s"initial config ${initial.id} must have quorum intersection"
    )

    OpenNetwork(
      bootstrapConfigId = initial.id,
      knownConfigs = Map(initial.id -> initial),
      knownTransitions = Map.empty,
      transitionVotes = Set.empty,
      enactedConfigs = Set(initial.id)
    )

  /** Keep the transition with the greater target configuration ID. */
  given Lattice[ConfigTransition] with
    override def merge(
        left: ConfigTransition,
        right: ConfigTransition
    ): ConfigTransition =
      if left.to >= right.to then left else right

  /** Transition votes are immutable facts stored in a set. */
  given Lattice[ConfigTransitionVote] with
    override def merge(
        left: ConfigTransitionVote,
        right: ConfigTransitionVote
    ): ConfigTransitionVote =
      left

  /** Merge replicated network knowledge component-wise. */
  given Lattice[OpenNetwork] with
    override def merge(left: OpenNetwork, right: OpenNetwork): OpenNetwork =
      OpenNetwork(
        bootstrapConfigId =
          Math.min(left.bootstrapConfigId, right.bootstrapConfigId),
        knownConfigs =
          Lattice.merge(left.knownConfigs, right.knownConfigs),
        knownTransitions =
          Lattice.merge(left.knownTransitions, right.knownTransitions),
        transitionVotes =
          left.transitionVotes union right.transitionVotes,
        enactedConfigs =
          left.enactedConfigs union right.enactedConfigs
      )


/** A requested network reconfiguration. */
sealed trait ReconfigOp:
    def nextId: ConfigId


/** Add a node and optionally update existing slices. */
final case class AddNode(
    nextId: ConfigId,
    node: Uid,
    nodeSlices: QuorumSlices,
    updatedExistingSlices: QuorumConfig = Map.empty
) extends ReconfigOp


/** Remove a node and provide slices for the remaining members. */
final case class RemoveNode(
    nextId: ConfigId,
    node: Uid,
    replacementSlices: QuorumConfig
) extends ReconfigOp


/** Update quorum slices without changing membership. */
final case class UpdateSlices(
    nextId: ConfigId,
    updatedSlices: QuorumConfig
) extends ReconfigOp
