package ex2024textRDT.fugue

import rdts.base.Uid
import rdts.time.{Causal, Dots}
import ex2024textRDT.*

import scala.collection.mutable

/** In-memory replication between replicas in the same process, for tests.
  *
  * Each replica remembers all operation batches it knows (its own and received ones), so syncing forwards batches
  * transitively. Delivery goes through [[Replica.receive]], i.e., the same causal delivery as with a real network.
  */
object InMemorySync {

  private type Batch = (Dots, Causal[Vector[Any]])

  // property tests run in parallel; replicas are compared by identity, as equal-looking replicas are distinct
  private val known =
    java.util.Collections.synchronizedMap(java.util.IdentityHashMap[Replica[?], mutable.ArrayBuffer[Batch]]()).nn

  private def log(replica: Replica[?]): mutable.ArrayBuffer[Batch] =
    known.computeIfAbsent(replica, _ => mutable.ArrayBuffer.empty).nn

  /** Assign a dot to the local operations of `replica` that were not yet sent. */
  def flush(replica: Replica[?]): Unit =
    replica.broadcast { delta =>
      val dot = replica.causalBuffer.applied.nextDot(Uid.predefined(replica.state.replicaId))
      log(replica).addOne((Dots.single(dot), delta.asInstanceOf[Causal[Vector[Any]]]))
      Some(dot)
    }

  def sync(a: Replica[?], b: Replica[?]): Unit = {
    syncFrom(a, b)
    syncFrom(b, a)
  }

  def syncFrom(to: Replica[?], from: Replica[?]): Unit = {
    flush(from)
    val toLog = log(to)
    log(from).toList.foreach { (dots, delta) =>
      if !toLog.exists(_._1 == dots) then {
        toLog.addOne((dots, delta))
        to.receive(dots, delta.asInstanceOf[Causal[to.Ops]])
      }
    }
  }
}

given replicaAlgorithm[F <: FugueFactory]: CollaborativeTextEditingAlgorithm[Replica[F]] with {
  extension (replica: Replica[F]) {
    def insert(i: Int, x: Char): Unit = replica.insert(i, x)

    def delete(i: Int): Unit = replica.delete(i)

    def text(): String = replica.text()

    def sync(other: Replica[F]): Unit = {
      syncFrom(other)
      other.syncFrom(replica)
    }

    def syncFrom(other: Replica[F]): Unit = InMemorySync.syncFrom(replica, other)
  }
}
