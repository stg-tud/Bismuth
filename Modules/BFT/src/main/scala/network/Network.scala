package network

import java.security.PublicKey
import scala.collection.immutable.Queue

object Network:
  var channels: Map[Array[Byte], Queue[Array[Byte]]] = Map.empty

  def put(receiver: Array[Byte], msg: Array[Byte]): Unit =
    val queue = channels.getOrElse(receiver, Queue.empty).enqueue(msg)
    channels = channels.updated(receiver, queue)

  def get(replicaID: Array[Byte]): Queue[Array[Byte]] =
    val result = channels.getOrElse(replicaID, Queue.empty)
    channels = channels.updated(replicaID, Queue.empty)

    result
