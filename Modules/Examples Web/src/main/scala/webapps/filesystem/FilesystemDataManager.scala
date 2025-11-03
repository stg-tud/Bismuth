package webapps.filesystem

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
import rdts.experiments.UndoRedoReplica

object FilesystemDataManager {

  case class FilesystemRepState(buffer: UndoRedoReplica.Buffer[FilesystemState])

  given JsonValueCodec[FilesystemRepState] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  val CBR(receivedCallback, dataManager: DeltaDissemination[FilesystemRepState]) = Event.fromCallback {
    DeltaDissemination[FilesystemRepState](Filesystem.replicaId, Event.handle)
  }

  def resetBuffer[A] = Fold.Branch[UndoRedoReplica[A]](Nil, isStatic = false, _ => Fold.current.clearBuffer())

  type State = UndoRedoReplica[FilesystemState];

  def hookup(
      init: State,
  )(create: (State, Fold.Branch[State]) => Signal[State]) = {
    dataManager.lock.synchronized {
      dataManager.applyDelta(FilesystemRepState(init.buffer))
      val fullInit =
        dataManager.allPayloads.foldLeft(init)((s, d) => s.receive(d.payload.data.buffer))

      val branch = Fold.branch[State] {
        receivedCallback.value match
          case None    => Fold.current
          case Some(v) => Fold.current.receive(v.buffer)
      }

      val sig = create(fullInit, branch)

      sig.observe { delta =>
        dataManager.applyDelta(FilesystemRepState(delta.buffer))
      }

      sig
    }
  }

}
