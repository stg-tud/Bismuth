package test.rdts.bespoke

import rdts.experiments.AuctionInterface.{AuctionData, Bid}
import rdts.experiments.AuctionInterface
import rdts.syntax.{DeltaBuffer, DeltaBufferContainer}

class DottedTest extends munit.FunSuite {

  test("AuctionData can be in DeltaBuffer") {
    val auction: DeltaBuffer[AuctionData] = DeltaBuffer(AuctionData.empty)

    assert(auction.state.bids == Set.empty)
    assert(auction.state.status == AuctionInterface.Open)
    assert(auction.state.winner == None)

    val added = auction.mod(_.bid("First", 1))
    assert(added.state.bids == Set(Bid("First", 1)))
    assert(added.state.status == AuctionInterface.Open)
    assert(added.state.winner == None)

    val knockedDown = added.mod(_.knockDown())
    assert(knockedDown.state.bids == Set(Bid("First", 1)))
    assert(knockedDown.state.status == AuctionInterface.Closed)
    assert(knockedDown.state.winner == Some("First"))
  }

  test("AuctionData can be in DeltaBufferContainer") {
    val auction: DeltaBufferContainer[AuctionData] = DeltaBufferContainer(DeltaBuffer(AuctionData.empty))

    assert(auction.result.state.bids == Set.empty)
    assert(auction.result.state.status == AuctionInterface.Open)
    assert(auction.result.state.winner == None)

    val added = auction.mod(_.bid("First", 1))
    assert(added.result.state.bids == Set(Bid("First", 1)))
    assert(added.result.state.status == AuctionInterface.Open)
    assert(added.result.state.winner == None)

    val knockedDown = added.mod(_.knockDown())
    assert(knockedDown.result.state.bids == Set(Bid("First", 1)))
    assert(knockedDown.result.state.status == AuctionInterface.Closed)
    assert(knockedDown.result.state.winner == Some("First"))
  }

}
