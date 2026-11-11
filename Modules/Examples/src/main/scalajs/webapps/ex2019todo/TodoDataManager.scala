package webapps.ex2019todo

import channels.JsoniterCodecs.given
import channels.MessagingIO
import webapps.OverlayReplication
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

  val (receivedCallback, replication: OverlayReplication[TodoRepState]) = Event.fromCallback {
    OverlayReplication[TodoRepState](Todolist.replicaId, delta => Event.handle(delta))
  }

  def dataManager: MessagingIO[?] = replication.io

  def hookup[A: Lattice](
      init: A,
      wrap: A => TodoRepState,
      unwrap: TodoRepState => Option[A]
  )(create: (A, Fold.Branch[DeltaBuffer[A]]) => Signal[DeltaBuffer[A]]): Signal[DeltaBuffer[A]] = {
    replication.broadcast(wrap(init))
    val fullInit =
      replication.appPayloads.flatMap(unwrap).foldLeft(init)(Lattice.merge)

    val branch = Fold.branch[DeltaBuffer[A]] {
      receivedCallback.value.flatMap(unwrap) match
          case None    => Fold.current
          case Some(v) => Fold.current.applyDeltaNonAppend(v)
    }

    val sig = create(fullInit, branch)

    sig.observe { buffer =>
      buffer.deltaBuffer.foreach { delta =>
        replication.broadcast(wrap(delta))
      }
    }

    sig
  }

}
