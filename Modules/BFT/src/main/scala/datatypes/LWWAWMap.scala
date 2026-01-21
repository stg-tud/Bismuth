package datatypes

import crypto.Ed25519Util
import dag.{Event, HashDAG}

import scala.util.hashing.MurmurHash3

case class LWWAWMap[K, V](
    mapTagged: Map[K, Set[(V, String)]],
    hashDAG: HashDAG[MapOperation[K, V]]
) extends Replica[MapOperation[K, V], LWWAWMap[K, V]]:

    def put(key: K, value: V): LWWAWMap[K, V] =
        val op = AddKeyValue(key, value)

        LWWAWMap(Map.empty, hashDAG.generateDelta(op))

    def remove(key: K): LWWAWMap[K, V] =
      if !this.contains(key) then
          LWWAWMap(Map.empty, hashDAG.empty)
      else
          val op = RemoveKey[K, V](key)

          LWWAWMap(Map.empty, hashDAG.generateDelta(op))

    def map: Map[K, V] = mapTagged.map((k, set) => (k, choseValueRandomlyAndDeterministically(set)._1))

    private def choseValueRandomlyAndDeterministically(set: Set[(V, String)]): (V, String) =
      set.toList.sortWith((x, y) => MurmurHash3.stringHash(x._2) > MurmurHash3.stringHash(y._2)).head

    def get(key: K): Option[V] = map.get(key)

    def contains(key: K): Boolean = map.contains(key)

    def keyset: Set[K] = map.keySet

    def values: Iterable[V] = map.values

    override def merge(other: LWWAWMap[K, V]): LWWAWMap[K, V] = {
      var newMap        = this.mapTagged
      val newHashDAG    = this.hashDAG.merge(other.hashDAG)
      var orderedEvents = List.empty[Event[MapOperation[K, V]]]

      for event <- this.hashDAG.queue ++ other.hashDAG.events.values ++ other.hashDAG.queue do
          if newHashDAG.contains(event) && !this.hashDAG.contains(event) then
              orderedEvents = event :: orderedEvents

      orderedEvents = newHashDAG.orderEvents(orderedEvents)

      for event <- orderedEvents do
          if newHashDAG.contains(event) && !this.hashDAG.contains(event) then
              val op = event.content.get
              op match
                  case AddKeyValue(key, value) =>
                    if newMap.contains(key) then
                        var currentValues = newMap(key)
                        for (v, id) <- currentValues do
                            if newHashDAG.pathExists(id, event.id) then
                                currentValues = currentValues -- Set((v, id))

                        newMap = newMap + (key -> (currentValues ++ Set((value, event.id))))
                    else {
                      newMap = newMap + (key -> Set((value, event.id)))
                    }
                  case RemoveKey(k) =>
                    if newMap.contains(k) then
                        var currentValues = newMap(k)
                        for (v, id) <- currentValues do
                            if newHashDAG.pathExists(id, event.id) then
                                currentValues = currentValues -- Set((v, id))

                        if currentValues.isEmpty then
                            newMap = newMap - k
                        else
                            newMap = newMap + (k -> currentValues)

      LWWAWMap(newMap, newHashDAG)
    }

    def empty: LWWAWMap[K, V] = LWWAWMap()

    def withHashDAG(hashDAG: HashDAG[MapOperation[K, V]]): LWWAWMap[K, V] = this.copy(hashDAG = hashDAG)

    override def generateDelta(ids: List[String]): LWWAWMap[K, V] =
      LWWAWMap(Map.empty, hashDAG.getDelta(ids))

object LWWAWMap:
    def apply[K, V](): LWWAWMap[K, V] = {
      val keyPair = Ed25519Util.generateNewKeyPair
      new LWWAWMap[K, V](
        Map.empty,
        HashDAG[MapOperation[K, V]](keyPair.getPublic, Some(keyPair.getPrivate))
      )
    }

sealed trait MapOperation[K, V]
case class AddKeyValue[K, V](key: K, value: V) extends MapOperation[K, V]
case class RemoveKey[K, V](key: K)             extends MapOperation[K, V]
