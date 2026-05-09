package replication

import channels.{ConnectionDescriptor, LocalConnectionRegistry, LocalMessageQueue, PeerConnectInfo, QueuedLocalConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.LocalUid
import replication.overlay.HyParViewStateMachine
import replication.overlay.HyParViewStateMachine.HyParViewConfig

import scala.util.Random

class BroadcastIOIntegrationTest extends munit.FunSuite {

  given JsonValueCodec[Set[String]] = JsonCodecMaker.make

  final case class Node(id: String, io: BroadcastIO[Set[String]], selfInfo: PeerConnectInfo)

  private def runLineNetwork(
      n: Int,
      config: Option[HyParViewConfig] = Some(
        HyParViewConfig(
          activeViewSize = 2,
          passiveViewSize = 12,
          activeRandomWalkLength = 3,
          passiveRandomWalkLength = 1,
          shuffleRandomWalkLength = 2,
          shuffleActiveSample = 1,
          shufflePassiveSample = 3,
        )
      ),
      rounds: Int = 500,
  ): Vector[Node] = {
    require(n >= 3, s"need at least 3 nodes, got $n")

    val queue          = LocalMessageQueue()
    val links          = (0 until n).map(i => s"n$i" -> QueuedLocalConnection(queue)).toMap
    val resolver       = LocalConnectionRegistry(links)
    val networkConfig  = config.getOrElse(HyParViewConfig.fromEstimatedNetworkSize(n))

    val nodes = (0 until n).toVector.map { i =>
      val uid      = LocalUid.gen()
      val id       = s"n$i"
      val selfInfo = PeerConnectInfo(uid.uid, Set(ConnectionDescriptor.QueuedLocal(id)))
      val random   = Random(0xB15 + i)
      val overlay  = HyParViewStateMachine.empty(selfInfo, networkConfig, random.between, _ => true)
      val io = BroadcastIO[Set[String]](
        uid,
        _ => (),
        overlay = Some(overlay),
        resolver = resolver,
      )
      Node(id, io, selfInfo)
    }

    nodes.foreach { node =>
      node.io.addBinaryConnection(resolver.queuedServer(node.selfInfo.channelConnectors.head).get)
    }

    nodes.indices.foreach { i =>
      val lineNeighbors = Vector(i - 1, i + 1)
        .filter(j => j >= 0 && j < nodes.size)
        .map(nodes(_).selfInfo)
        .toVector
      lineNeighbors.foreach(nodes(i).io.bootstrapVia)
    }

    def drainQueue(maxSteps: Int = 2000): Unit = {
      var steps = 0
      while queue.nonEmpty && steps < maxSteps do
          queue.deliverAll()
          steps += 1
      assert(steps < maxSteps, s"queue did not quiesce, remaining=${queue.size}")
    }

    def settle(ticks: Int): Unit = {
      (0 until ticks).foreach { _ =>
        drainQueue()
        nodes.foreach(_.io.repairTick())
      }
      drainQueue()
    }

    settle(rounds)
    nodes
  }

  test("BroadcastIO with HyParView grows a line into a well-connected overlay") {
    val n     = 8
    val nodes = runLineNetwork(n = n)

    val activeSizes = nodes.map { node =>
      node.io.overlayController.asInstanceOf[HyParViewStateMachine].activeView.size
    }

    val targetActive = math.min(
      nodes.head.io.overlayController.asInstanceOf[HyParViewStateMachine].config.activeViewSize,
      n - 1,
    )
    val minimumExpected = math.max(1, targetActive - 1)

    activeSizes.foreach { size =>
      assert(
        size >= minimumExpected,
        s"expected active view size >= $minimumExpected (target=$targetActive), got $size; sizes=$activeSizes"
      )
    }
    assert(
      activeSizes.count(_ >= targetActive) >= n - 2,
      s"expected most nodes to reach the target active view size $targetActive, got $activeSizes"
    )
  }
}
