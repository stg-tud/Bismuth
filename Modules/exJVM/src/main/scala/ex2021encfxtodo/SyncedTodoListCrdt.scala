package ex2021encfxtodo

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import ex2021encfxtodo.SyncedTodoListCrdt.{StateType, stateCodec1}
import rdts.base.LocalUid
import rdts.syntax.oldCompat.DeltaAWLWWMContainer

import java.util.UUID
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.duration.{DurationInt, MILLISECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

class SyncedTodoListCrdt(
    val replicaId: LocalUid,
    onUpdated: (Map[UUID, TodoEntry], Map[UUID, TodoEntry]) => Unit = (_, _) => (),
) {

  private val crdt: DeltaAWLWWMContainer[UUID, TodoEntry] =
    new DeltaAWLWWMContainer[UUID, TodoEntry](replicaId)

  private val crdtExecutorService: ExecutorService = Executors.newSingleThreadExecutor()
  private val crdtExecContext: ExecutionContext    = ExecutionContext.fromExecutor(crdtExecutorService)

  private val connectionManager: ConnectionManager[StateType] =
    ConnectionManager[StateType](replicaId, handleStateReceived)

  def address: String = "Connect via signaling server"

  def connect(connectionString: String): Unit =
    connectionManager.connectToSignalingServer(connectionString)

  protected def handleStateReceived(state: StateType): Unit = {
    runInCrdtExecContext { () =>
      val before = crdt.values
      crdt.merge(state)
      val after = crdt.values
      onUpdated(before, after)
    }
  }

  protected def queryCrdtState(): StateType = runInCrdtExecContext(() => crdt.state)

  private def runInCrdtExecContext[Ret](op: () => Ret): Ret = Await.result[Ret](
    Future {
      op()
    }(using crdtExecContext),
    100.milliseconds
  )

  def shutdown(): Unit = {
    connectionManager.stop()
    crdtExecutorService.shutdown()
    crdtExecutorService.awaitTermination(500, MILLISECONDS)
    ()
  }

  def get(key: UUID): Option[TodoEntry] =
    runInCrdtExecContext(() => crdt.get(key))

  def put(key: UUID, value: TodoEntry): Unit = {
    val delta = runInCrdtExecContext { () =>
      crdt.putDelta(key, value)
    }
    connectionManager.stateChanged(delta)
  }

  def remove(key: UUID): Unit = {
    val delta = runInCrdtExecContext { () =>
      crdt.removeDelta(key)
    }
    connectionManager.stateChanged(delta)
  }

  def values: Map[UUID, TodoEntry] =
    runInCrdtExecContext(() => crdt.values)

  def remoteAddresses: Set[String] = connectionManager.remoteAddresses
}

object SyncedTodoListCrdt {
  type StateType = DeltaAWLWWMContainer.State[UUID, TodoEntry]

  given stateCodec1: JsonValueCodec[StateType] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true).withMapAsArray(true))

}
