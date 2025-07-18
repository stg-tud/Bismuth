package webapps.todo

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.{Bottom, Lattice}
import rdts.datatypes.{LastWriterWins, ReplicatedList}
import rdts.syntax.DeltaBuffer
import rdts.time.Dots
import reactives.default.*
import reactives.operator.Event.CBR
import replication.DeltaDissemination
import replication.JsoniterCodecs.given

object TodoDataManager {

  case class TodoRepState(list: ReplicatedList[TaskRef], entries: Map[String, LastWriterWins[Option[TaskData]]])

  given JsonValueCodec[TodoRepState] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given Lattice[TodoRepState]        = Lattice.derived
  given Bottom[TodoRepState]         = Bottom.derived

  val CBR(receivedCallback, dataManager: DeltaDissemination[TodoRepState]) = Event.fromCallback {
    DeltaDissemination[TodoRepState](Todolist.replicaId, Event.handle)
  }

  def hookup[A: Lattice](
      init: A,
      wrap: A => TodoRepState,
      unwrap: TodoRepState => Option[A]
  )(create: (A, Fold.Branch[DeltaBuffer[A]]) => Signal[DeltaBuffer[A]]) = {
    dataManager.lock.synchronized {
      dataManager.applyDelta(wrap(init))
      val fullInit =
        dataManager.allPayloads.flatMap(v => unwrap(v.payload.data)).foldLeft(init)(Lattice.merge)

      val branch = Fold.branch[DeltaBuffer[A]] {
        receivedCallback.value.flatMap(unwrap) match
          case None    => Fold.current
          case Some(v) => Fold.current.applyDeltaNonAppend(v)
      }

      val sig = create(fullInit, branch)

      sig.observe { buffer =>
        buffer.deltaBuffer.foreach { delta =>
          dataManager.applyDelta(wrap(delta))
        }
      }

      sig
    }
  }

}
