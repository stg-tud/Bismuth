package probench.clients

import probench.Codecs.given
import probench.data.RequestResponseQueue.Timestamp
import probench.data.{KVOperation, RequestResponseQueue}
import rdts.base.{LocalUid, Uid}
import replication.DeltaDissemination

import java.util.concurrent.Semaphore
import scala.collection.mutable
import scala.concurrent.{Future, Promise}

class ProBenchClient(val name: Uid, blocking: Boolean = true, logTimings: Boolean) extends Client(name, logTimings) {
  type State = RequestResponseQueue[KVOperation[String, String], String]

  given localUid: LocalUid = LocalUid(name)

  val writeDataManager: DeltaDissemination[State] =
    DeltaDissemination[State](localUid, handleIncomingWrite, defaultTimetolive = 1)
  val readDataManager: DeltaDissemination[State] =
    DeltaDissemination[State](localUid, handleIncomingRead, defaultTimetolive = 1)

  inline def log(inline msg: String): Unit =
    if true then println(s"[$name] $msg")

  val requestSemaphore = new Semaphore(0)

  var writeQueue: State      = RequestResponseQueue.empty
  var readQueue: State      = RequestResponseQueue.empty
  val currentStateLock: AnyRef = new {}

  val promises: mutable.HashMap[Timestamp, Promise[String]] = mutable.HashMap.empty[Timestamp, Promise[String]]

  def readWithResult(key: String): Future[String] =
      val (timestamp, queue) = readQueue.request(KVOperation.Read(key))
      publishRead(queue)
      val p = Promise[String]()
      promises.synchronized {
        promises.put(timestamp, p)
      }
      p.future

  def writeWithResult(key: String, value: String): Future[String] =
      val (timestamp, queue) = writeQueue.request(KVOperation.Write(key, value))
      publishWrite(queue)
      val p = Promise[String]()
      promises.synchronized {
        promises.put(timestamp, p)
      }
      p.future

  def publishWrite(delta: State): State = currentStateLock.synchronized {
    if delta `inflates` writeQueue then {
      log("publishing write")
      writeQueue = writeQueue.merge(delta)
      writeDataManager.applyDelta(delta)
    } else
        log("skip")
    writeQueue
  }
  def publishRead(delta: State): State = currentStateLock.synchronized {
    if delta `inflates` readQueue then {
      log("publishing read")
      readQueue = readQueue.merge(delta)
      readDataManager.applyDelta(delta)
    } else
      log("skip")
    readQueue
  }

  def handleIncomingWrite(change: State): Unit = currentStateLock.synchronized {
    log(s"handling incoming write: $change")
    val (old, changed) = currentStateLock.synchronized {
      val old = writeQueue
      writeQueue = writeQueue `merge` change
      (old, writeQueue)
    }
    if old != changed then {
      assert(changed == writeQueue)
      maybeHandleWriteResponses(changed)
      // else log(s"upkept: ${pprint(upkept)}")
    }
  }

  def handleIncomingRead(change: State): Unit = currentStateLock.synchronized {
    log(s"handling incoming read: $change")
    val (old, changed) = currentStateLock.synchronized {
      val old = readQueue
      readQueue = readQueue `merge` change
      (old, readQueue)
    }
    if old != changed then {
      assert(changed == readQueue)
      maybeHandleReadResponses(changed)
      // else log(s"upkept: ${pprint(upkept)}")
    }
  }

  private def maybeHandleWriteResponses(newState: State): Unit =
      val (requests, responses) = (newState.requests, newState.responses)
      // println(s"open promises: $promises")
      currentStateLock.synchronized {
        promises.synchronized {
          for {
            timestamp <- promises.keys
          } {
            responses.get(timestamp) match
                case Some(res) =>
                  onResultValue(res.value)
                  promises.remove(timestamp) match {
                    case Some(promise) =>
                      promise.success(res.value)
                      publishWrite(writeQueue.receive(timestamp))
                    case None => ()
                  }
                  if blocking then requestSemaphore.release(1)
                case None => ()
          }
        }
      }

  private def maybeHandleReadResponses(newState: State): Unit =
    val (requests, responses) = (newState.requests, newState.responses)
    // println(s"open promises: $promises")
    currentStateLock.synchronized {
      promises.synchronized {
        for {
          timestamp <- promises.keys
        } {
          responses.get(timestamp) match
            case Some(res) =>
              onResultValue(res.value)
              promises.remove(timestamp) match {
                case Some(promise) =>
                  promise.success(res.value)
                  publishRead(readQueue.receive(timestamp))
                case None => ()
              }
              if blocking then requestSemaphore.release(1)
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
