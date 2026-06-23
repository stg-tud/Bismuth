package rdts.protocols.chimeric

import rdts.base.{Lattice, Uid}

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
            slices = Lattice.merge(left.slices,right.slices)
          )
        else if left.id > right.id then left
        else right

final case class OpenNetwork(
    currentConfigId: ConfigId,
    configs: Map[ConfigId, NetworkConfig]
):
    def currentConfig: NetworkConfig =
      configs(currentConfigId)

    def config(id: ConfigId): NetworkConfig =
      configs(id)

    def stage(next: NetworkConfig): OpenNetwork =
      require(!configs.contains(next.id), s"config ${next.id} already exists")
      require(next.id > currentConfigId, s"new config id must be larger than current")
      require(
        FBASOpen.isSafeTransition(currentConfig.slices, next.slices),
        s"unsafe transition from config $currentConfigId to config ${next.id}"
      )
      copy(configs = configs + (next.id -> next))

    def activate(nextId: ConfigId): OpenNetwork =
      require(configs.contains(nextId), s"unknown config $nextId")
      require(nextId >= currentConfigId, s"cannot move backward from $currentConfigId to $nextId")
      copy(currentConfigId = nextId)

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

object OpenNetwork:
    def bootstrap(initial: NetworkConfig): OpenNetwork =
      require(
        FBASOpen.hasQuorumIntersection(initial.slices),
        s"initial config ${initial.id} must have quorum intersection"
      )
      OpenNetwork(
        currentConfigId = initial.id,
        configs = Map(initial.id -> initial)
      )

    given Lattice[OpenNetwork] with
      override def merge(left: OpenNetwork, right: OpenNetwork): OpenNetwork =
        OpenNetwork(
          currentConfigId = Math.max(left.currentConfigId, right.currentConfigId),
          configs = Lattice.merge(left.configs, right.configs)
        )