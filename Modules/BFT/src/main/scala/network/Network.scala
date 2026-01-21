package network

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap, LinkedBlockingQueue}

object Network:
    private val channels: ConcurrentMap[String, LinkedBlockingQueue[Array[Byte]]] = ConcurrentHashMap()

    def startChannel(ReplicaID: String): Unit =
      channels.putIfAbsent(ReplicaID, LinkedBlockingQueue()): Unit

    def put(receiver: String, msg: Array[Byte]): Unit =
      channels.get(receiver).put(msg)

    def get(replicaID: String): Array[Byte] =
      channels.get(replicaID).take()
