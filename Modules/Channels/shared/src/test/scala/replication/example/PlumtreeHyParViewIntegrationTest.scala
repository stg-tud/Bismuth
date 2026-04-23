package replication.example

import channels.{LocalMessageQueue, QueuedLocalConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{LocalUid, Uid}
import replication.{PlumtreeDissemination, ProtocolMessage}
import replication.overlay.{HyParViewOverlay, HyParViewOverlayNode}
import replication.overlay.HyParViewOverlay.HyParViewConfig

import scala.util.Random

class PlumtreeHyParViewIntegrationTest extends munit.FunSuite {

  test("hyparview drives plumtree dissemination") {
    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    val n = 100
    val random = Random(7)

    val controlQueue = LocalMessageQueue[HyParViewOverlay.HyParViewMessage]()
    val dataQueue    = LocalMessageQueue[ProtocolMessage[Set[String]]]()

    val ids = Vector.tabulate(n)(i => Uid.predefined(s"hp$i"))

    val controlNetwork = ids.map(id => id -> QueuedLocalConnection[HyParViewOverlay.HyParViewMessage](controlQueue)).toMap

    def edgeKey(a: Uid, b: Uid): (Uid, Uid) = if Uid.unwrap(a) <= Uid.unwrap(b) then (a, b) else (b, a)

    val dataNetwork =
      (for
          i <- ids.indices
          j <- (i + 1) until ids.size
      yield {
        val a = ids(i)
        val b = ids(j)
        edgeKey(a, b) -> QueuedLocalConnection[ProtocolMessage[Set[String]]](dataQueue)
      }).toMap

    val dms = ids.map(_ => PlumtreeDissemination[Set[String]](LocalUid.gen(), _ => (), None))

    val cfg = HyParViewConfig.fromEstimatedNetworkSize(n)

    val nodes = ids.zipWithIndex.map { case (id, idx) =>
      val contact = if idx == 0 then None else Some(ids.head)
      HyParViewOverlayNode(id, controlNetwork, dataNetwork, dms(idx), contact, random, cfg)
    }

    nodes.foreach(_.startServer())
    nodes.foreach(_.join())

    var safety = 0
    while (controlQueue.nonEmpty || dataQueue.nonEmpty) && safety < 200000 do
      if controlQueue.nonEmpty then controlQueue.deliverAll()
      if dataQueue.nonEmpty then dataQueue.deliverAll()
      safety += 1

    (0 until 5).foreach { _ =>
      nodes.foreach(_.shuffleTick())
      var inner = 0
      while (controlQueue.nonEmpty || dataQueue.nonEmpty) && inner < 200000 do
        if controlQueue.nonEmpty then controlQueue.deliverAll()
        if dataQueue.nonEmpty then dataQueue.deliverAll()
        inner += 1
    }

    dms.zipWithIndex.foreach { case (dm, i) => dm.applyDelta(Set(s"x$i")) }

    var dissemination = 0
    while (controlQueue.nonEmpty || dataQueue.nonEmpty) && dissemination < 200000 do
      println(s"dissemination $dissemination control: ${controlQueue.size} data: ${dataQueue.size} ")
      if controlQueue.nonEmpty then controlQueue.deliverAll()
      if dataQueue.nonEmpty then dataQueue.deliverAll()
      dissemination += 1

    val expected = ids.indices.map(i => Set(s"x$i")).toSet

    dms.foreach { dm =>
      assertEquals(dm.allPayloads.map(_.payload.data).toSet, expected)
    }
  }
}
