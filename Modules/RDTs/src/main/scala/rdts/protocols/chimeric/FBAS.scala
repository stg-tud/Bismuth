package rdts.protocols.chimeric

import rdts.base.Uid
import rdts.protocols.Voting

type QuorumSlices = Set[Set[Uid]]
type QuorumConfig = Map[Uid, Set[Set[Uid]]]

object FBAS:

  def isQuorumReached(
    config: QuorumConfig,
    voters: Set[Uid]
  ): Boolean =
    config.nonEmpty &&
    voters == config.keySet &&
    voters.forall { node =>
      config.get(node).exists { slices =>
        slices.exists(slice => slice.subsetOf(voters))
      }
    }

  def getVotersFor[A](value: A, proposals: Voting[A]): Set[Uid] =
    proposals.votes
      .filter(_.value == value)
      .map(_.voter)