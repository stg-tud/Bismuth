package probench.clients

import probench.data
import probench.data.ClientComm.given
import probench.data.Codecs.given
import probench.data.{ClientCommRead, ClientCommWrite, KVOperation}
import rdts.base.{LocalUid, Uid}
import replication.DeltaStorage.Type
import replication.{PlumtreeDissemination, DeltaStorage}

import java.util.concurrent.Semaphore
import scala.collection.mutable
import scala.concurrent.{Future, Promise}

class ProBenchClient(val name: Uid, blocking: Boolean = true, logTimings: Boolean) extends Client(name, logTimings) {

  given localUid: LocalUid = LocalUid(name)

  val writeDataManager: PlumtreeDissemination[ClientCommWrite] =
    PlumtreeDissemination(
      localUid,
      handleIncomingWrite,
      defaultTimetolive = 1,
      deltaStorage = DeltaStorage.getStorage(Type.KeepAll, () => ???)
    )
  val readDataManager: PlumtreeDissemination[ClientCommRead] =
    PlumtreeDissemination(
      localUid,
      handleIncomingRead,
      defaultTimetolive = 1,
      deltaStorage = DeltaStorage.getStorage(Type.KeepAll, () => ???)
    )

  inline def log(inline msg: String): Unit =
    if false then println(s"[$name] $msg")

  val requestSemaphore = new Semaphore(0)

  val currentStateLock: AnyRef = new {}

  private val promises: mutable.HashMap[Uid, Promise[String]] = mutable.HashMap.empty[Uid, Promise[String]]

  def readWithResult(key: String): Future[String] =
      val id                              = Uid.gen()
      val request: ClientCommRead.ReadReq = ClientCommRead.ReadReq(id, KVOperation.Read(key))
      val p                               = Promise[String]()
      promises.synchronized {
        promises.put(id, p)
      }
      publishRead(request)
      p.future

  def writeWithResult(key: String, value: String): Future[String] = currentStateLock.synchronized {
    val id                                = Uid.gen()
    val request: ClientCommWrite.WriteReq = ClientCommWrite.WriteReq(id, KVOperation.Write(key, value))
    val p                                 = Promise[String]()
    promises.synchronized {
      promises.put(id, p)
      log("adding promise")
    }
    publishWrite(request)
    p.future
  }

  def publishWrite(delta: ClientCommWrite.WriteReq): Unit = currentStateLock.synchronized {
    log("publishing write")
    writeDataManager.applyDelta(delta)
  }
  def publishRead(delta: ClientCommRead.ReadReq): Unit = currentStateLock.synchronized {
    log("publishing read")
    readDataManager.applyDelta(delta)
  }

  def handleIncomingWrite(change: ClientCommWrite): Unit = currentStateLock.synchronized {
    log(s"handling incoming write: $change")
    maybeHandleWriteResponses(change)
  }

  def handleIncomingRead(change: ClientCommRead): Unit = currentStateLock.synchronized {
    log(s"handling incoming read: $change")
    maybeHandleReadResponses(change)
  }

  private def maybeHandleWriteResponses(update: ClientCommWrite): Unit =
    update match {
      case data.ClientCommWrite.WriteReq(id, kvOperation) => ()
      case data.ClientCommWrite.WriteRes(id, value)       =>
        log(s"handling write responses promises: ${promises.size}: $update")
        promises.synchronized {
          onResultValue(value)
          promises.remove(id) match {
            case Some(promise) =>
              promise.success(value): Unit
            case None => ()
          }
        }
    }

  private def maybeHandleReadResponses(update: ClientCommRead): Unit =
    update match {
      case data.ClientCommRead.ReadReq(id, kvOperation) => ()
      case data.ClientCommRead.ReadRes(id, value)       =>
        log(s"handling read responses promises: ${promises.size}: $update")
        promises.synchronized {
          onResultValue(value)
          promises.remove(id) match {
            case Some(promise) =>
              promise.success(value): Unit
            case None => ()
          }
        }
    }

  override def handleOpImpl(op: KVOperation[String, String]): Unit =
    // TODO: still not sure that the semaphore use is correct …
    // its quite likely possible that some other request is answered after draining, causing the code below to return immediately
    // though overall currentOp is not protected at all, so it is triple unclear what is going on

    op match {
      case KVOperation.Read(key)         => readWithResult(key): Unit
      case KVOperation.Write(key, value) => writeWithResult(key, value): Unit
    }

}
