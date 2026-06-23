package rdts.protocols.chimeric

import rdts.base.Uid
import rdts.protocols.Voting

object FBASOpen:

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

    def quorums(config: QuorumConfig): Set[Set[Uid]] =
      powerSet(config.keySet).filter(voters => isQuorumReached(config, voters))

    def hasQuorumIntersection(config: QuorumConfig): Boolean =
      val qs = quorums(config).toVector
      qs.indices.forall { i =>
        ((i + 1) until qs.length).forall { j =>
          qs(i).intersect(qs(j)).nonEmpty
        }
      }

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

    def getVotersFor[A](value: A, proposals: Voting[A]): Set[Uid] =
      proposals.votes
        .filter(_.value == value)
        .map(_.voter)

    private def powerSet[A](s: Set[A]): Set[Set[A]] =
      s.foldLeft(Set(Set.empty[A])) { (acc, elem) =>
        acc ++ acc.map(_ + elem)
      }.filter(_.nonEmpty)