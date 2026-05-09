package probench.data

import rdts.base.{LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.protocols.Participants

class HeartbeatQuorumTest extends munit.FunSuite {

  test("Quorum example scenario") {

    val nums = Range(1, 6)
    val ids  = nums.map(i => s"id$i").map(Uid.predefined)
    given Participants(ids.toSet)

    val heartbeats =
      nums.map(i => (ids(i - 1), LastWriterWins.now(Heartbeat(Some(Uid.predefined("leader")), i, Some(i)))))
        .toMap

    val hbq = HeartbeatQuorum(heartbeats)

    assert(hbq.heartbeats.size == nums.size)

    given LocalUid(Uid.predefined("leader"))

    assertEquals(hbq.alivePeers(timeoutThreshold = 2, currentTime = 5), Set(Uid("id5"), Uid("id4"), Uid("id3")))
    assert(hbq.hasQuorum(timeoutThreshold = 2, currentTime = 5)(using LocalUid(Uid.predefined("leader"))))
    assertEquals(hbq.alivePeers(timeoutThreshold = 1, currentTime = 5), Set(Uid("id5"), Uid("id4")))
    assert(!hbq.hasQuorum(timeoutThreshold = 1, currentTime = 5)(using LocalUid(Uid.predefined("leader"))))

  }

}
