package datatypes

import crypto.Ed25519Util
import dag.HashDAG
import scala.util.hashing.MurmurHash3

case class LWWAWMap[K, V](
    map: Map[K, V],
    tags: Map[K, Set[String]],
    hashDAG: HashDAG[MapOperation[K, V]]
) extends Replica[MapOperation[K, V], LWWAWMap[K, V]]:

    def put(key: K, value: V): LWWAWMap[K, V] =
        val op = AddKeyValue(key, value)

        LWWAWMap(Map.empty, Map.empty, hashDAG.generateDelta(op))

    def remove(key: K): LWWAWMap[K, V] =
        val op = RemoveKey[K, V](key)

        LWWAWMap(Map.empty, Map.empty, hashDAG.generateDelta(op))

    def get(key: K): Option[V] = map.get(key)

    def contains(key: K): Boolean = map.contains(key)

    def keyset: Set[K] = map.keySet

    def values: Iterable[V] = map.values

    override def merge(other: LWWAWMap[K, V]): LWWAWMap[K, V] = {
      var newMap     = this.map
      var newTags    = this.tags
      val newHashDAG = this.hashDAG.merge(other.hashDAG)

      for event <- this.hashDAG.queue ++ other.hashDAG.events.values ++ other.hashDAG.queue do
          if newHashDAG.contains(event) && !this.hashDAG.contains(event) then
              val op = event.content.get
              op match
                  case AddKeyValue(key, value) =>
                    if newMap.contains(key) then
                        var ids = Set.empty[String]
                        for id <- newTags(key) do
                            if !newHashDAG.pathExists(id, event.id) then
                                ids = ids + id

                        ids = ids + event.id
                        val chosenID =
                          ids.toList.sortWith((x, y) => MurmurHash3.stringHash(x) > MurmurHash3.stringHash(y)).head
                        val chosenEvent = newHashDAG.getEventByID(chosenID)
                        val v           = chosenEvent.content.get match
                            case AddKeyValue(k, v) => v
                            case RemoveKey(_) => throw Exception("This is not supposed to happen")

                        newMap = newMap + (key   -> v)
                        val newSet: Set[String]   = newTags.getOrElse(key, Set.empty) + event.id
                        newTags = newTags + (key -> newSet)
                    else {
                      newMap = newMap + (key   -> value)
                      val newSet: Set[String]   = newTags.getOrElse(key, Set.empty) + event.id
                      newTags = newTags + (key -> newSet)
                    }
                  case RemoveKey(k) =>
                    if !newMap.contains(k) || !newTags.contains(k) then {
                      newMap = newMap - k
                      newTags = newTags - k
                    } else
                        for id <- newTags(k) do
                            if newHashDAG.pathExists(id, event.id) then
                                newTags = newTags + (k -> (newTags(k) - id))

                        if !newTags.contains(k) || newTags(k).isEmpty then
                            newMap = newMap - k

      LWWAWMap(newMap, newTags, newHashDAG)
    }

    def empty: LWWAWMap[K, V] = LWWAWMap()

    def withHashDAG(hashDAG: HashDAG[MapOperation[K, V]]): LWWAWMap[K, V] = this.copy(hashDAG = hashDAG)

    override def generateDelta(ids: List[String]): LWWAWMap[K, V] =
      LWWAWMap(Map.empty, Map.empty, hashDAG.getDelta(ids))

object LWWAWMap:
    def apply[K, V](): LWWAWMap[K, V] = {
      val keyPair = Ed25519Util.generateNewKeyPair
      new LWWAWMap[K, V](
        Map.empty,
        Map.empty,
        HashDAG[MapOperation[K, V]](keyPair.getPublic, Some(keyPair.getPrivate))
      )
    }

sealed trait MapOperation[K, V]
case class AddKeyValue[K, V](key: K, value: V) extends MapOperation[K, V]
case class RemoveKey[K, V](key: K)        extends MapOperation[K, V]
