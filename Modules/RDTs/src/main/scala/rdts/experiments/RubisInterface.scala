package rdts.experiments

import rdts.base.{LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import AuctionInterface.Bid.User

import scala.util.chaining.scalaUtilChainingOps

/** A Rubis (Rice University Bidding System) is a Delta CRDT modeling an auction system.
  *
  * Bids can only be placed on auctions that were previously opened and with a previously registered userId. When an auction
  * is closed, concurrently placed bids are still accepted and may thus change the winner of the auction. To prevent two
  * replicas from concurrently registering the same userId, requests for registering a new userId must be resolved by a
  * central replica using resolveRegisterUser.
  *
  * This auction system was in part modeled after the Rice University Bidding System (RUBiS) proposed by Cecchet et al. in
  * "Performance and Scalability of EJB Applications", see [[https://www.researchgate.net/publication/2534515_Performance_and_Scalability_of_EJB_Applications here]]
  */
object RubisInterface {
  type AID = String

  case class State(current: (ReplicatedSet[(User, Uid)], Map[User, Uid], Map[AID, AuctionInterface.AuctionData])) {

    type Delta = State

    def placeBid(auctionId: AID, userId: User, price: Int)(using LocalUid): Delta = {
      val (_, users, m) = current
      val newMap        =
        if users.get(userId).contains(LocalUid.replicaId) && m.contains(auctionId) then {
          m.updatedWith(auctionId) {
            _.map(a => a.bid(userId, price))
          }
        } else Map.empty[AID, AuctionInterface.AuctionData]

      deltaState.make(auctions = newMap)
    }

    def closeAuction(auctionId: AID): Delta = {
      val (_, _, m) = current
      val newMap    =
        if m.contains(auctionId) then {
          m.updatedWith(auctionId) {
            _.map(a => a.knockDown())
          }
        } else Map.empty[AID, AuctionInterface.AuctionData]

      deltaState.make(auctions = newMap)
    }

    def openAuction(auctionId: AID): Delta = {
      val (_, _, m) = current
      val newMap    =
        if m.contains(auctionId) then Map.empty[AID, AuctionInterface.AuctionData]
        else Map(auctionId -> AuctionInterface.AuctionData())

      deltaState.make(auctions = newMap)
    }

    def requestRegisterUser(using LocalUid)(userId: User): Delta = {
      val (req, users, _) = current
      if users.contains(userId) then deltaState.make()
      else
        val merged = req.add(userId -> LocalUid.replicaId)
        deltaState.make(userRequests = merged)
    }

    def resolveRegisterUser(): Delta = {
      val (req, users, _) = current
      val newUsers        = req.elements.foldLeft(Map.empty[User, Uid]) {
        case (newlyRegistered, (uid, rid)) =>
          if (users ++ newlyRegistered).contains(uid) then
            newlyRegistered
          else {
            newlyRegistered.updated(uid, rid)
          }
      }

      req.clear().pipe { ur =>
        deltaState.make(
          userRequests = ur,
          users = newUsers
        )
      }

    }
  }

  private def deltaState: DeltaStateFactory = new DeltaStateFactory

  private class DeltaStateFactory {
    val bottom: State = State(ReplicatedSet.empty, Map.empty, Map.empty)

    def make(
        userRequests: ReplicatedSet[(User, Uid)] = bottom.current._1,
        users: Map[User, Uid] = bottom.current._2,
        auctions: Map[AID, AuctionInterface.AuctionData] = bottom.current._3
    ): State = State(userRequests, users, auctions)
  }
}
