package network

import java.security.PublicKey
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.immutable.Queue

object Network:
  var channels: Map[String, LinkedBlockingQueue[Array[Byte]]] = Map.empty

  def put(receiver: String, msg: Array[Byte]): Unit =
    val queue = channels.getOrElse(receiver, LinkedBlockingQueue())
    queue.put(msg)
    channels = channels.updated(receiver, queue)

  def get(replicaID: String): Array[Byte] =
    val queue = channels.getOrElse(replicaID, LinkedBlockingQueue())
    channels = channels.updated(replicaID, queue)

    queue.take()
