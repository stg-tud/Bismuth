package rdts.protocols.chimeric

import rdts.base.Uid
import rdts.protocols.Voting

/** Quorum and transition-safety operations for an FBAS configuration. */
object FBASOpen:

    /** Check whether a set of voters satisfies every member's slice requirement. */
    def isQuorumReached(
        config: QuorumConfig,
        voters: Set[Uid]
    ): Boolean =
      config.nonEmpty &&
      voters.nonEmpty &&
      voters.subsetOf(config.keySet) &&
      voters.forall { node =>
        config.get(node).exists { slices =>
          slices.exists(slice => slice.subsetOf(voters))
        }
      }

    /** Enumerate all non-empty quorums of a configuration. */
    def quorums(
        config: QuorumConfig
    ): Set[Set[Uid]] =
      powerSet(config.keySet)
        .filter(voters => isQuorumReached(config, voters))

    /** Check whether every pair of quorums intersects. */
    def hasQuorumIntersection(
        config: QuorumConfig
    ): Boolean =
        val qs =
          quorums(config).toVector

        qs.indices.forall { i =>
          ((i + 1) until qs.length).forall { j =>
            qs(i).intersect(qs(j)).nonEmpty
          }
        }

    /** Check quorum intersection within and across two configurations. */
    def isSafeTransition(
        oldConfig: QuorumConfig,
        newConfig: QuorumConfig
    ): Boolean =
      hasQuorumIntersection(oldConfig) &&
      hasQuorumIntersection(newConfig) &&
      quorums(oldConfig).forall { oldQ =>
        quorums(newConfig).forall { newQ =>
          oldQ.intersect(newQ).nonEmpty
        }
      }

    /** Return the voters that support a particular proposal value. */
    def getVotersFor[A](
        value: A,
        proposals: Voting[A]
    ): Set[Uid] =
      proposals.votes
        .filter(_.value == value)
        .map(_.voter)

    /** Generate all non-empty subsets of a set. */
    private def powerSet[A](
        s: Set[A]
    ): Set[Set[A]] =
      s.foldLeft(Set(Set.empty[A])) { (acc, elem) =>
        acc ++ acc.map(_ + elem)
      }.filter(_.nonEmpty)
