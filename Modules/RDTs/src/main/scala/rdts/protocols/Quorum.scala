package rdts.protocols

import rdts.base.Uid
import rdts.protocols.Participants.*

trait Quorum:
    def isQuorum(votes: Set[Uid])(using Participants): Boolean

object Quorum {
  def isQuorum(votes: Set[Uid])(using q: Quorum, p: Participants): Boolean = q.isQuorum(votes)

  case object FullQuorum extends Quorum:
      def isQuorum(votes: Set[Uid])(using Participants): Boolean = votes == participants
}
