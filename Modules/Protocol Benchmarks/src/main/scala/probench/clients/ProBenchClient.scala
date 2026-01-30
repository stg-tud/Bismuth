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

  val dataManager: DeltaDissemination[State] =
    DeltaDissemination[State](localUid, handleIncoming, defaultTimetolive = 1)

  inline def log(inline msg: String): Unit =
    if false then println(s"[$name] $msg")

  val requestSemaphore = new Semaphore(0)

  var currentState: State      = RequestResponseQueue.empty
  val currentStateLock: AnyRef = new {}

  val promises: mutable.HashMap[Timestamp, Promise[String]] = mutable.HashMap.empty[Timestamp, Promise[String]]

  def readWithResult(key: String): Future[String] =
      val (timestamp, queue) = currentState.request(KVOperation.Read(key))
      transform(_ => queue)
      val p = Promise[String]()
      promises.synchronized {
        promises.put(timestamp, p)
      }
      p.future

  def writeWithResult(key: String, value: String): Future[String] =
      val (timestamp, queue) = currentState.request(KVOperation.Write(key, value))
      transform(_ => queue)
      val p = Promise[String]()
      promises.synchronized {
        promises.put(timestamp, p)
      }
      p.future

  def publish(delta: State): State = currentStateLock.synchronized {
    if delta `inflates` currentState then {
      log("publishing")
      currentState = currentState.merge(delta)
      dataManager.applyDelta(delta)
    } else
        log("skip")
    currentState
  }

  def transform(f: State => State): State = currentStateLock.synchronized {
    publish(f(currentState))
  }

  def handleIncoming(change: State): Unit = currentStateLock.synchronized {
    log("handling incoming")
    val (old, changed) = currentStateLock.synchronized {
      val old = currentState
      currentState = currentState `merge` change
      (old, currentState)
    }
    if old != changed then {
      assert(changed == currentState)
      maybeHandleResponses(changed)
      // else log(s"upkept: ${pprint(upkept)}")
    }
  }

  private def maybeHandleResponses(newState: State): Unit =
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
                      transform(_.receive(timestamp))
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
