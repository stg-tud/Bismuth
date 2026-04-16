package replication.example

import channels.{LocalMessageQueue, QueuedLocalConnection, SynchronousLocalConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.LocalUid

import replication.{DeltaDissemination, ProtocolMessage}

class DeltaDisseminationTest extends munit.FunSuite {
  test("basics") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    // I have no clue why this syntax is still not deprecated xD
    val dd1, dd2, dd3 = DeltaDissemination[Set[String]](LocalUid.gen(), _ => (), None, defaultTimetolive = Int.MaxValue)

    val sync = SynchronousLocalConnection[ProtocolMessage[Set[String]]]()

    dd2.addObjectConnection(sync.client("2"))
    dd1.addObjectConnection(sync.server)
    dd3.addObjectConnection(sync.client("3"))

    dd1.pingAll()
    dd2.pingAll()
    dd3.pingAll()

    dd1.applyDelta(Set("a"))
    dd2.applyDelta(Set("b"))
    dd3.applyDelta(Set("c"))

    assertEquals(
      dd1.allPayloads.map(_.payload.data).toSet,
      dd2.allPayloads.map(_.payload.data).toSet
    )
    assertEquals(
      dd2.allPayloads.map(_.payload.data).toSet,
      dd3.allPayloads.map(_.payload.data).toSet
    )

  }

  test("basics queued") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    val dd1, dd2, dd3 = DeltaDissemination[Set[String]](LocalUid.gen(), _ => (), None, defaultTimetolive = Int.MaxValue)

    val queue  = LocalMessageQueue[ProtocolMessage[Set[String]]]()
    val queued = QueuedLocalConnection[ProtocolMessage[Set[String]]](queue)

    dd2.addObjectConnection(queued.client("2"))
    dd1.addObjectConnection(queued.server)
    dd3.addObjectConnection(queued.client("3"))

    def deliverAndPrint(): Unit =
      val log = false
      if log then
        println("delivering:")
        queue.elements.foreach(e => println(s"  $e"))
      queue.deliverAll()
      if log then
        println("enqueued: ")
        queue.elements.foreach(e => println(s"  $e"))
        println("")

    deliverAndPrint()
    dd1.pingAll()
    deliverAndPrint()
    dd2.pingAll()
    dd3.pingAll()
    deliverAndPrint()



    dd1.applyDelta(Set("a"))
    deliverAndPrint()
    dd2.applyDelta(Set("b"))
    deliverAndPrint()

    dd3.applyDelta(Set("c"))
    deliverAndPrint()
    deliverAndPrint()
    deliverAndPrint()

    assertEquals(
      dd1.allPayloads.map(_.payload.data).toSet,
      dd2.allPayloads.map(_.payload.data).toSet
    )
    assertEquals(
      dd2.allPayloads.map(_.payload.data).toSet,
      dd3.allPayloads.map(_.payload.data).toSet
    )

  }

}
