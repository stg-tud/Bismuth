package webapps.ex2019todo

import channels.broadcast.PlumtreeBroadcast

import channels.MessagingIO
import channels.JsoniterCodecs.given
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.{Bottom, Lattice}
import rdts.datatypes.{LastWriterWins, ReplicatedList}
import rdts.syntax.DeltaBuffer
import rdts.time.Dots
import reactives.default.*

object TodoDataManager {

  case class TodoRepState(list: ReplicatedList[TaskRef], entries: Map[String, LastWriterWins[Option[TaskData]]])

  given JsonValueCodec[TodoRepState] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given Lattice[TodoRepState]        = Lattice.derived
  given Bottom[TodoRepState]         = Bottom.derived

  val (receivedCallback, dataManager: MessagingIO.Plumtree[TodoRepState]) = Event.fromCallback {
    MessagingIO[TodoRepState](Todolist.replicaId, payload => Event.handle(payload.data))
  }

  def hookup[A: Lattice](
      init: A,
      wrap: A => TodoRepState,
      unwrap: TodoRepState => Option[A]
  )(create: (A, Fold.Branch[DeltaBuffer[A]]) => Signal[DeltaBuffer[A]]): Signal[DeltaBuffer[A]] = {
    dataManager.broadcast(wrap(init))
    val fullInit =
      dataManager.allPayloads.flatMap(v => unwrap(v.data)).foldLeft(init)(Lattice.merge)

    val branch = Fold.branch[DeltaBuffer[A]] {
      receivedCallback.value.flatMap(unwrap) match
          case None    => Fold.current
          case Some(v) => Fold.current.applyDeltaNonAppend(v)
    }

    val sig = create(fullInit, branch)

    sig.observe { buffer =>
      buffer.deltaBuffer.foreach { delta =>
        dataManager.broadcast(wrap(delta))
      }
    }

    sig
  }

}
