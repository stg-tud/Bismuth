package webapps.ex2024textRDT

import channels.connection.{LocalMessageQueue, QueuedLocalConnection}
import channels.{BroadcastIO, JsoniterCodecs, Timers}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import rdts.base.ReplicaId
import rdts.time.{Causal, Dots}
import text_rdt.{FugueFactory, Replica}

/** Replicates a Fugue [[Replica]] over a [[BroadcastIO]].
  *
  * Local operations are collected by the replica and sent as one batch on [[publish]]. Received batches are applied
  * in causal order by the replica.
  */
final class TextReplication[F <: FugueFactory](val replica: Replica[F]) {

  given JsonValueCodec[Causal[replica.Ops]] =
    TextReplication.causalCodec(using JsoniterCodecs.CausalContextCodec)(using
      TextReplication.vectorCodec(using replica.state.factoryContext.messageCodec)
    )

  /** Timers only fire on [[fireTimers]]; the demos have no need for background repair. */
  val timers: Timers.Manual = Timers.Manual()

  val io: BroadcastIO[Causal[replica.Ops]] = BroadcastIO[Causal[replica.Ops]](
    ReplicaId.predefined(replica.state.replicaId),
    payload => replica.receive(payload.dots, payload.data),
    timers = timers,
  )

  /** Send all local operations since the last call. */
  def publish(): Unit = replica.broadcast(io.broadcast)

  def fireTimers(): Int = timers.fireAll()
}

object TextReplication {

  /** An in-memory connection between two replications whose messages are only delivered on [[sync]]. */
  final class LocalLink(a: TextReplication[?], b: TextReplication[?]) {
    private val queue = LocalMessageQueue()
    private val link  = QueuedLocalConnection(s"${a.replica.state.replicaId}-${b.replica.state.replicaId}", queue)

    a.io.addServerConnection(link.server)
    b.io.addClientConnection(link.client(b.replica.state.replicaId))

    /** Exchange messages until both sides have nothing more to say. Messages forwarded to other links stay queued.
      * Bounded, because repair timers keep firing while a peer announces dots that it can no longer provide.
      */
    def sync(): Unit = {
      var progress = true
      var rounds   = 0
      while progress && rounds < 100 do {
        rounds += 1
        a.publish()
        b.publish()
        progress = queue.deliverAll() > 0
        progress = a.fireTimers() > 0 || progress
        progress = b.fireTimers() > 0 || progress
      }
    }
  }

  def vectorCodec[A](using codec: JsonValueCodec[A]): JsonValueCodec[Vector[A]] = new JsonValueCodec[Vector[A]] {
    override def decodeValue(in: JsonReader, default: Vector[A]): Vector[A] =
      if !in.isNextToken('[') then in.decodeError("expected [")
      else if in.isNextToken(']') then Vector.empty
      else {
        in.rollbackToken()
        val builder = Vector.newBuilder[A]
        while {
          builder += codec.decodeValue(in, codec.nullValue)
          in.isNextToken(',')
        } do ()
        if !in.isCurrentToken(']') then in.arrayEndOrCommaError()
        builder.result()
      }

    override def encodeValue(x: Vector[A], out: JsonWriter): Unit = {
      out.writeArrayStart()
      x.foreach(codec.encodeValue(_, out))
      out.writeArrayEnd()
    }

    override def nullValue: Vector[A] = Vector.empty
  }

  def causalCodec[A](using dotsCodec: JsonValueCodec[Dots])(using
      codec: JsonValueCodec[A]
  ): JsonValueCodec[Causal[A]] = new JsonValueCodec[Causal[A]] {
    override def decodeValue(in: JsonReader, default: Causal[A]): Causal[A] = {
      if !in.isNextToken('[') then in.decodeError("expected [")
      val predecessors = dotsCodec.decodeValue(in, dotsCodec.nullValue)
      if !in.isNextToken(',') then in.commaError()
      val value = codec.decodeValue(in, codec.nullValue)
      if !in.isNextToken(']') then in.arrayEndError()
      Causal(predecessors, value)
    }

    override def encodeValue(x: Causal[A], out: JsonWriter): Unit = {
      out.writeArrayStart()
      dotsCodec.encodeValue(x.predecessors, out)
      codec.encodeValue(x.value, out)
      out.writeArrayEnd()
    }

    override def nullValue: Causal[A] = null.asInstanceOf[Causal[A]]
  }
}
