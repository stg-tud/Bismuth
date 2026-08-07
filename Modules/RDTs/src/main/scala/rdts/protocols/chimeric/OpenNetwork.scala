package rdts.protocols.chimeric

import rdts.base.{Lattice, LocalUid, Uid}

type ConfigId = Long

final case class NetworkConfig(
    id: ConfigId,
    members: Set[Uid],
    slices: QuorumConfig
):
    require(members.nonEmpty, s"config $id must have at least one member")
    require(slices.keySet == members, s"config $id must define slices for every member")
    require(slices.values.flatten.flatten.toSet.subsetOf(members), s"config $id has slices containing unknown members")
    require(
      slices.forall { case (uid, ss) =>
        ss.nonEmpty && ss.forall(slice => slice.nonEmpty && slice.contains(uid))
      },
      s"config $id requires every node to have non-empty self-containing slices"
    )

object NetworkConfig:
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

final case class ConfigTransition(
    from: ConfigId,
    to: ConfigId,
    next: NetworkConfig
)

final case class ConfigTransitionVote(
    from: ConfigId,
    to: ConfigId,
    voter: Uid
)

final case class OpenNetwork(
    bootstrapConfigId: ConfigId,
    knownConfigs: Map[ConfigId, NetworkConfig],
    knownTransitions: Map[(ConfigId, ConfigId), ConfigTransition],
    transitionVotes: Set[ConfigTransitionVote],
    enactedConfigs: Set[ConfigId]
):
    def currentConfigId: ConfigId =
      enactedConfigs.maxOption.getOrElse(bootstrapConfigId)

    def currentConfig: NetworkConfig =
      knownConfigs(currentConfigId)

    def config(id: ConfigId): NetworkConfig =
      knownConfigs(id)

    def knowsConfig(id: ConfigId): Boolean =
      knownConfigs.contains(id)

    def knowsTransition(from: ConfigId, to: ConfigId): Boolean =
      knownTransitions.contains((from, to))

    def knowConfig(cfg: NetworkConfig): OpenNetwork =
      copy(knownConfigs = knownConfigs + (cfg.id -> cfg))

    def knowTransition(t: ConfigTransition): OpenNetwork =
        require(t.next.id == t.to, s"transition target ${t.to} must match next config id ${t.next.id}")
        require(knownConfigs.contains(t.from), s"unknown source config ${t.from}")
        require(t.to > t.from, s"transition target ${t.to} must be greater than source ${t.from}")
        require(
          FBASOpen.isSafeTransition(config(t.from).slices, t.next.slices),
          s"unsafe transition from config ${t.from} to ${t.to}"
        )
        copy(
          knownConfigs = knownConfigs + (t.to                   -> t.next),
          knownTransitions = knownTransitions + ((t.from, t.to) -> t)
        )

    def voteTransition(from: ConfigId, to: ConfigId)(using LocalUid): OpenNetwork =
        require(knownTransitions.contains((from, to)), s"unknown transition ($from -> $to)")
        copy(
          transitionVotes = transitionVotes + ConfigTransitionVote(from, to, summon[LocalUid].uid)
        )

    def votersFor(from: ConfigId, to: ConfigId): Set[Uid] =
      transitionVotes.collect {
        case ConfigTransitionVote(`from`, `to`, voter) => voter
      }

    def transitionDecision(from: ConfigId): Option[ConfigId] =
      knownTransitions.keys
        .collect { case (`from`, to) => to }
        .toList
        .sorted
        .find { to =>
          FBASOpen.isQuorumReached(config(from).slices, votersFor(from, to))
        }

    def enact(to: ConfigId): OpenNetwork =
        require(knownConfigs.contains(to), s"unknown config $to")
        copy(enactedConfigs = enactedConfigs + to)

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

    def deriveConfigWithoutNode(
        nextId: ConfigId,
        node: Uid,
        replacementSlices: QuorumConfig
    ): NetworkConfig =
        require(currentConfig.members.contains(node), s"node $node not present in current config")
        NetworkConfig(
          id = nextId,
          members = currentConfig.members - node,
          slices = replacementSlices
        )

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

    def proposeTransition(next: NetworkConfig): ConfigTransition =
      ConfigTransition(
        from = currentConfigId,
        to = next.id,
        next = next
      )

object OpenNetwork:
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

    given Lattice[ConfigTransition] with
        override def merge(left: ConfigTransition, right: ConfigTransition): ConfigTransition =
          if left.to >= right.to then left else right

    given Lattice[ConfigTransitionVote] with
        override def merge(left: ConfigTransitionVote, right: ConfigTransitionVote): ConfigTransitionVote =
          left

    given Lattice[OpenNetwork] with
        override def merge(left: OpenNetwork, right: OpenNetwork): OpenNetwork =
          OpenNetwork(
            bootstrapConfigId = Math.min(left.bootstrapConfigId, right.bootstrapConfigId),
            knownConfigs = Lattice.merge(left.knownConfigs, right.knownConfigs),
            knownTransitions = Lattice.merge(left.knownTransitions, right.knownTransitions),
            transitionVotes = left.transitionVotes union right.transitionVotes,
            enactedConfigs = left.enactedConfigs union right.enactedConfigs
          )

sealed trait ReconfigOp:
    def nextId: ConfigId

final case class AddNode(
    nextId: ConfigId,
    node: Uid,
    nodeSlices: QuorumSlices,
    updatedExistingSlices: QuorumConfig = Map.empty
) extends ReconfigOp

final case class RemoveNode(
    nextId: ConfigId,
    node: Uid,
    replacementSlices: QuorumConfig
) extends ReconfigOp

final case class UpdateSlices(
    nextId: ConfigId,
    updatedSlices: QuorumConfig
) extends ReconfigOp
