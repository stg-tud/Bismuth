package benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import datatypes.Replica
import network.Network
import dag.Event

class PingPongSync[T, R <: Replica[T, R]](
                                         var replica: R,
                                         val id: String,
                                         val otherID: String,
                                         val stats: SyncStats
                                       )(using JsonValueCodec[R]) extends Thread:

  sealed trait SyncMsg
  case class HeadsMsg(heads: Set[String]) extends SyncMsg
  case class MissingRequest(ids: Set[String]) extends SyncMsg
  case class Delta(delta: R) extends SyncMsg
  case class Done() extends SyncMsg
  //given c1: JsonValueCodec[HeadsMsg] = JsonCodecMaker.make
  //given c2: JsonValueCodec[MissingRequest] = JsonCodecMaker.make
  //given c3: JsonValueCodec[Delta] = JsonCodecMaker.make
  //given c4: JsonValueCodec[Done] = JsonCodecMaker.make
  given JsonValueCodec[SyncMsg] = JsonCodecMaker.make(CodecMakerConfig.withDiscriminatorFieldName(Some("type")))

  override def run(): Unit =
    Network.startChannel(id)
    val myHeads = replica.hashDAG.getCurrentHeadsIDs
    val myIDs = replica.hashDAG.getIDs
    Network.put(otherID, writeToArray[SyncMsg](HeadsMsg(myHeads)))
    stats.addBandwidth(myHeads.size)
    stats.incMessageCounter()

    var synced = false
    var otherIsDone = false
    while !synced || !otherIsDone do
      readFromArray[SyncMsg](Network.get(this.id)) match
        case HeadsMsg(otherHeads) =>
          val missing    = otherHeads -- myIDs

          if missing.isEmpty then {
            Network.put(otherID, writeToArray[SyncMsg](Done()))
            stats.incMessageCounter()
            synced = true
          } else {
            Network.put(otherID, writeToArray[SyncMsg](MissingRequest(missing)))
            stats.incMessageCounter()
            stats.addBandwidth(missing.size)
          }

        case MissingRequest(ids: Set[String]) =>
          Network.put(otherID, writeToArray[SyncMsg](Delta(replica.generateDelta(ids.toList))))
          stats.incMessageCounter()
          stats.addBandwidth(ids.size)

        case Delta(delta) =>
          this.replica = this.replica.merge(delta)
          val events = delta.hashDAG.queue
          val dependencies = events.foldLeft(Set.empty[String])((x, y) => x ++ y.dependencies)
          val missing = dependencies.filterNot(id => replica.hashDAG.contains(id))
          if missing.nonEmpty then
            Network.put(otherID, writeToArray[SyncMsg](MissingRequest(missing)))
          else {
            Network.put(otherID, writeToArray[SyncMsg](Done()))
            synced = true
          }
          stats.incMessageCounter()
          stats.addBandwidth(missing.size)

        case Done() =>
          otherIsDone = true
