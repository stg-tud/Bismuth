package ex2024textRDT.fugue

import ex2024textRDT.*
import ex2024textRDT.fugue.complex.*
import ex2024textRDT.fugue.complexavl.*
import ex2024textRDT.fugue.simple.*
import ex2024textRDT.fugue.simpleavl.*
import rdts.base.Uid
import rdts.time.{Causal, Dot, Dots}

import scala.collection.mutable

/** Replication via [[Replica.broadcast]]/[[Replica.receive]] with a simulated dissemination layer that assigns dots and
  * delivers batches in arbitrary order (including duplicates).
  */
abstract class CausalBroadcastIntegrationTest(factory: RID => Replica[?]) extends munit.FunSuite {

  // erase the path-dependent operation type for the simulated network
  type AnyOps = Vector[Any]

  class Network {
    val sent: mutable.ArrayBuffer[(Dots, Causal[AnyOps])] = mutable.ArrayBuffer.empty

    def broadcast(replica: Replica[?]): Unit =
      replica.broadcast { delta =>
        val uid = Uid.predefined(replica.state.replicaId)
        val dot = Dot(uid, sent.count(_._1.contains(uid)).toLong)
        sent.addOne((Dots.single(dot), delta.asInstanceOf[Causal[AnyOps]]))
        Some(dot)
      }

    def deliver(replica: Replica[?], order: Iterable[(Dots, Causal[AnyOps])]): Unit =
      order.foreach((dots, delta) => replica.receive(dots, delta.asInstanceOf[Causal[replica.Ops]]))
  }

  extension (dots: Dots) def contains(uid: Uid): Boolean = dots.peers.contains(uid)

  def editorText(replica: Replica[?]): String = replica.editor.asInstanceOf[StringEditory].data.toString

  test("out of order delivery converges") {
    val net = Network()
    val a   = factory("A")
    val b   = factory("B")
    val c   = factory("C")

    a.insert(0, 'h')
    a.insert(1, 'i')
    net.broadcast(a)
    net.deliver(b, net.sent.toList)

    b.insert(2, '!')
    b.delete(0)
    net.broadcast(b)
    a.insert(2, '?')
    net.broadcast(a)

    // c receives everything reversed and duplicated
    net.deliver(c, (net.sent ++ net.sent).reverse)
    assertEquals(c.causalBuffer.pending, Nil)

    net.deliver(a, net.sent.toList)
    net.deliver(b, net.sent.toList.reverse)

    assertEquals(a.text(), b.text())
    assertEquals(b.text(), c.text())
    assertEquals(editorText(a), a.text())
    assertEquals(editorText(b), b.text())
    assertEquals(editorText(c), c.text())
    assert(a.text().contains('!'))
    assert(!a.text().contains('h'))
  }

  test("nothing to send") {
    val net = Network()
    val a   = factory("A")
    net.broadcast(a)
    assertEquals(net.sent.size, 0)
  }
}

private def causalReplica[F <: FugueFactory](using f: F)(replicaId: RID): Replica[?] =
  Replica(ReplicaState(replicaId)(using f), StringEditory())

class SimpleCausalBroadcastIntegrationTest
    extends CausalBroadcastIntegrationTest(causalReplica(using SimpleFugueFactory.simpleFugueFactory))
class ComplexCausalBroadcastIntegrationTest
    extends CausalBroadcastIntegrationTest(causalReplica(using ComplexFugueFactory.complexFugueFactory))
class SimpleAVLCausalBroadcastIntegrationTest
    extends CausalBroadcastIntegrationTest(causalReplica(using SimpleAVLFugueFactory.simpleAVLFugueFactory))
class ComplexAVLCausalBroadcastIntegrationTest
    extends CausalBroadcastIntegrationTest(causalReplica(using ComplexAVLFugueFactory.complexAVLFugueFactory))
