package rdts.experiments

import rdts.base.{Bottom, Lattice}

object AuctionInterface {
  sealed trait Status
  case object Open   extends Status
  case object Closed extends Status

  object Status {
    given lattice: Lattice[Status] = Lattice.sumLattice
  }

  case class Bid(userId: User, bid: Int)

  type User = String

  case class AuctionData(
      bids: Set[Bid] = Set.empty,
      status: Status = Open,
  ) {
    def bid(userId: User, price: Int): AuctionData =
      AuctionData(bids = Set(Bid(userId, price)))

    def knockDown(): AuctionData = AuctionData(status = Closed)

    lazy val winner: Option[User] = status match {
      case Open   => None
      case Closed => bids.maxByOption(_.bid).map(_.userId)
    }

  }

  object AuctionData {

    val empty: AuctionData = AuctionData()

    given bottom: Bottom[AuctionData] with { override def empty: AuctionData = AuctionData.empty }

    given AuctionDataAsUIJDLattice: Lattice[AuctionData] = Lattice.derived

  }
}
