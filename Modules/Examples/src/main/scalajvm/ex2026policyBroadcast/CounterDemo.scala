package ex2026policyBroadcast

import channels.JsoniterCodecs.given
import channels.MessagingIO
import channels.connection.{LocalMessageQueue, Peer, QueuedLocalConnection}
import PolicyBroadcast.mutate
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{Bottom, Lattice, ReplicaId, Uid}
import rdts.datatypes.GrowOnlyCounter

/** Two counters, `a` may only be written by replica A, and `b` only by replica B. */
case class Counters(a: GrowOnlyCounter = GrowOnlyCounter.zero, b: GrowOnlyCounter = GrowOnlyCounter.zero)
    derives Lattice, Bottom {
  override def toString: String = s"a=${a.value} b=${b.value}"
}

object Counters {
  given JsonValueCodec[Counters] = JsonCodecMaker.make

  val A: Uid = Uid.predefined("A")
  val B: Uid = Uid.predefined("B")
  val C: Uid = Uid.predefined("C")

  /** A may only read and write counter a, B only counter b. C may read both, but write neither. */
  val policy: BroadcastPolicy[Counters] = new BroadcastPolicy[Counters] {
    private def untouched(counter: GrowOnlyCounter) = Bottom[GrowOnlyCounter].isEmpty(counter)

    def mayRead(state: Counters, reader: Peer, delta: Counters): Boolean =
      reader.uid match
          case A => untouched(delta.b)
          case B => untouched(delta.a)
          case _ => true

    def mayWrite(state: Counters, writer: Peer, delta: Counters): Boolean =
      writer.uid match
          case A => untouched(delta.b)
          case B => untouched(delta.a)
          case _ => untouched(delta.a) && untouched(delta.b)
  }
}

/** Three replicas A, B, C in a full mesh over local connections, replicating [[Counters]] with [[PolicyBroadcast]]. */
object CounterDemo {
  import Counters.*

  type Protocol = PolicyBroadcast.Protocol[Counters]

  def main(args: Array[String]): Unit = {
    val queue = LocalMessageQueue()

    def replica(id: Uid): MessagingIO[Protocol] =
      new MessagingIO[Protocol](using PolicyBroadcast.Protocol(policy), PolicyBroadcast.messageCodec[Counters])(
        ReplicaId(id),
        PolicyBroadcast.State(Counters()),
        state => println(s"  ${Uid.unwrap(id)} sees $state")
      )

    val a = replica(A)
    val b = replica(B)
    val c = replica(C)

    def settle(): Unit = while queue.nonEmpty do { queue.deliverAll(); () }

    def show(): Unit =
      List("A" -> a, "B" -> b, "C" -> c).foreach((name, io) => println(s"  $name: ${io.state.value}"))

    def write(name: String, io: MessagingIO[Protocol], mutator: Counters => Counters): Unit =
        println(s"$name writes")
        try io.mutate(mutator)
        catch case e: IllegalArgumentException => println(s"  rejected: ${e.getMessage}")
        settle()

    // A writes before anyone is connected, the others get it on connect
    given ReplicaId = ReplicaId(A)
    write("A", a, s => Counters(a = s.a.inc()))

    val linkA = QueuedLocalConnection("A", queue)
    val linkC = QueuedLocalConnection("C", queue)
    a.addServerConnection(linkA.server)
    c.addServerConnection(linkC.server)

    println("connecting C to A")
    c.addClientConnection(linkA.client("C"))
    settle()
    println("connecting B to C, B does not learn A's write from C")
    b.addClientConnection(linkC.client("B"))
    settle()
    show()

    write("B", b, s => Counters(b = s.b.inc()(using ReplicaId(B))))
    println("no forwarding: C knows B's write, but A does not")
    show()

    println("connecting B to A, neither may read the other")
    b.addClientConnection(linkA.client("B"))
    settle()
    write("A", a, s => Counters(a = s.a.add(2)))
    show()

    println("B tries to write counter a")
    write("B", b, s => Counters(a = s.a.inc()(using ReplicaId(B))))
    println("C tries to write counter b")
    write("C", c, s => Counters(b = s.b.inc()(using ReplicaId(C))))
    show()
    List(a, b, c).foreach(_.globalAbort.closeRequest = true)
  }
}
