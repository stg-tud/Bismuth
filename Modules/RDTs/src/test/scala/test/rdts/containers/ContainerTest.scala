package test.rdts.containers

import rdts.base.{Bottom, LocalUid}
import rdts.base.LocalUid.asId
import rdts.datatypes.{EnableWinsFlag, LastWriterWins, ReplicatedSet}
import rdts.datatypes.experiments.AuctionInterface
import rdts.datatypes.experiments.AuctionInterface.{AuctionData, Bid}
import rdts.dotted.Dotted
import rdts.syntax.{DeltaBuffer, DeltaBufferContainer}
import test.rdts.UtilHacks.*
import test.rdts.UtilHacks2.*

class ContainerTest extends munit.FunSuite {

  object helper {

    given r: LocalUid = "me".asId

    given bottomString: Bottom[String] with {
      override def empty: String = ""
    }

  }

  import helper.given

  // START EnableWinsFlag

  test("Dotted can contain contextual EnableWinsFlag") {
    val flag: Dotted[EnableWinsFlag] = Dotted.empty
    given LocalUid                   = "me".asId

    assertEquals(flag.data.read, false)

    val enabled = flag.modn(_.enable())
    assertEquals(enabled.data.read, true)

    val disabled = enabled.modn(_.disable())
    assertEquals(disabled.data.read, false)
  }


  test("Dotted DeltaBuffer can contain contextual EnableWinsFlag") {
    val flag: DeltaBuffer[EnableWinsFlag] = DeltaBuffer(EnableWinsFlag.empty)

    assertEquals(flag.state.read, false)

    val enabled = flag.mod(_.enable())
    assertEquals(enabled.state.read, true)

    val disabled = enabled.mod(_.disable())
    assertEquals(disabled.state.read, false)
  }

  test("Dotted DeltaBufferContainer can contain contextual EnableWinsFlag") {
    val flag: DeltaBufferContainer[EnableWinsFlag] = DeltaBufferContainer(DeltaBuffer(EnableWinsFlag.empty))

    assertEquals(flag.result.state.read, false)

    flag.mod(_.enable())
    assertEquals(flag.result.state.read, true)

    flag.mod(_.disable())
    assertEquals(flag.result.state.read, false)
  }

  // END EnableWinsFlag

  // START ReplicatedSet

  test("Dotted can contain contextual ReplicatedSet[String]") {
    val awSet: Dotted[ReplicatedSet[String]] = Dotted.empty

    assert(awSet.data.elements.isEmpty)

    val added = awSet.modn(_.add("First"))
    assertEquals(added.data.elements.size, 1)
    assert(added.data.elements.contains("First"))
    assert(added.data.contains("First"))

    val removed = added.modn(_.remove("First"))
    assert(removed.data.elements.isEmpty)
  }

  // NOTE: DeltaBuffer cannot contain contextual ReplicatedSet without Dotted, as ReplicatedSet needs a context

  test("Dotted DeltaBuffer can contain contextual ReplicatedSet[String]") {
    val awSet: DeltaBuffer[ReplicatedSet[String]] = DeltaBuffer(Bottom.empty)

    assert(awSet.state.elements.isEmpty)

    val added = awSet.mod(_.add("First"))
    assertEquals(added.state.elements.size, 1)
    assert(added.state.elements.contains("First"))
    assert(added.state.contains("First"))

    val removed = added.mod(_.remove("First"))
    assert(removed.state.elements.isEmpty)
  }

  test("Dotted DeltaBufferContainer can contain contextual ReplicatedSet[String]") {
    val awSet: DeltaBufferContainer[ReplicatedSet[String]] = DeltaBufferContainer(DeltaBuffer(Bottom.empty))

    assert(awSet.result.state.elements.isEmpty)

    awSet.mod(_.add("First"))
    assertEquals(awSet.result.state.elements.size, 1)
    assert(awSet.result.state.elements.contains("First"))
    assert(awSet.result.state.contains("First"))

    awSet.mod(_.remove("First"))
    assert(awSet.result.state.elements.isEmpty)
  }

  // END ReplicatedSet

  // START LastWriterWins

  test("Dotted can contain non-contextual LastWriterWins[String]") {
    val lww: Dotted[LastWriterWins[String]] = Dotted.empty

    assertEquals(lww.data.read, "")

    val added = lww.write("First")
    assertEquals(added.data.read, "First")

    val removed = added.write("")
    assertEquals(removed.data.read, "")
  }

  test("DeltaBuffer can contain non-contextual LastWriterWins[String]") {
    val lww: DeltaBuffer[LastWriterWins[String]] = DeltaBuffer(LastWriterWins.empty)

    assertEquals(lww.read, "")

    val added = lww.write("First")
    assertEquals(added.read, "First")

    val removed = added.write("")
    assertEquals(removed.read, "")
  }

  test("Dotted DeltaBuffer can contain non-contextual LastWriterWins[String]") {
    val lww: DeltaBuffer[Dotted[LastWriterWins[String]]] = DeltaBuffer(Dotted.empty)

    assertEquals(lww.data.read, "")

    val added = lww.write("First")
    assertEquals(added.data.read, "First")

    val removed = added.write("")
    assertEquals(removed.data.read, "")
  }

  test("Dotted DeltaBufferContainer can contain non-contextual LastWriterWins[String]") {
    val lww: DeltaBufferContainer[Dotted[LastWriterWins[String]]] = DeltaBufferContainer(DeltaBuffer(Dotted.empty))

    assertEquals(lww.data.read, "")

    lww.write("First")
    assertEquals(lww.data.read, "First")

    lww.write("")
    assertEquals(lww.data.read, "")
  }

  // END LastWriterWins

  // START AuctionData

  test("plain AuctionData without container returns deltas") {
    val auction: AuctionData = AuctionData.empty

    assertEquals(auction.bids, Set.empty)
    assertEquals(auction.status, AuctionInterface.Open)
    assertEquals(auction.winner, None)

    val added_delta = auction.bid("First", 1)
    assertEquals(added_delta.bids, Set(Bid("First", 1)))
    assertEquals(added_delta.status, AuctionInterface.Open)
    assertEquals(added_delta.winner, None)

    val added: AuctionData = auction `merge` added_delta

    val knockedDown_delta: AuctionData = added.knockDown()
    assertEquals(knockedDown_delta.bids, Set.empty)
    assertEquals(knockedDown_delta.status, AuctionInterface.Closed)
    assertEquals(knockedDown_delta.winner, None)

    val knockedDown: AuctionData = added `merge` knockedDown_delta

    assertEquals(knockedDown.bids, Set(Bid("First", 1)))
    assertEquals(knockedDown.status, AuctionInterface.Closed)
    assertEquals(knockedDown.winner, Some("First"))
  }

  test("Dotted can contain plain AuctionData") {
    val auction: Dotted[AuctionData] = Dotted.empty

    assertEquals(auction.data.bids, Set.empty)
    assertEquals(auction.data.status, AuctionInterface.Open)
    assertEquals(auction.data.winner, None)

    val added = auction.modn(_.bid("First", 1))
    assertEquals(added.data.bids, Set(Bid("First", 1)))
    assertEquals(added.data.status, AuctionInterface.Open)
    assertEquals(added.data.winner, None)

    val knockedDown = added `merge` added.modn(_.knockDown())
    assertEquals(knockedDown.data.bids, Set(Bid("First", 1)))
    assertEquals(knockedDown.data.status, AuctionInterface.Closed)
    assertEquals(knockedDown.data.winner, Some("First"))
  }

  test("Dotted DeltaBuffer can contain plain AuctionData") {
    val auction: DeltaBuffer[AuctionData] = DeltaBuffer(AuctionData.empty)

    assertEquals(auction.state.bids, Set.empty)
    assertEquals(auction.state.status, AuctionInterface.Open)
    assertEquals(auction.state.winner, None)

    val added = auction.mod(_.bid("First", 1))
    assertEquals(added.state.bids, Set(Bid("First", 1)))
    assertEquals(added.state.status, AuctionInterface.Open)
    assertEquals(added.state.winner, None)

    val knockedDown = added.mod(_.knockDown())
    assertEquals(knockedDown.state.bids, Set(Bid("First", 1)))
    assertEquals(knockedDown.state.status, AuctionInterface.Closed)
    assertEquals(knockedDown.state.winner, Some("First"))
  }

  // END AuctionData

}
