package datatypes

import crypto.Ed25519Util
import dag.HashDAG

import scala.util.hashing.MurmurHash3

case class LWWGrowOnlyList[T](
    itemsMap: Map[Int, (T, Set[String])],
    hashDAG: HashDAG[ListItem[T]]
) extends Replica[ListItem[T], LWWGrowOnlyList[T]]:

    lazy val list: List[T] =
      itemsMap.toList.sortWith((item1, item2) => item1._1 <= item2._1).map(x => x._2).filter((elem, tags) =>
        tags.nonEmpty
      ).map((elem, tags) => elem)

    def add(index: Int, element: T): LWWGrowOnlyList[T] = {
      val l = list
      if index > l.size || index < 0 then
          throw Exception("illegal index")

      var hashDAGDelta = hashDAG.generateDelta(ListItem(element, index))
      var newHashDAG   = hashDAG.merge(hashDAGDelta)
      for i <- index until l.size do
          val delta = newHashDAG.generateDelta(ListItem(l(i), i + 1))
          newHashDAG = newHashDAG.merge(delta)
          hashDAGDelta = hashDAGDelta.merge(delta)

      LWWGrowOnlyList(Map.empty, hashDAGDelta)
    }

    override def merge(other: LWWGrowOnlyList[T]): LWWGrowOnlyList[T] =
        val newHashDAG  = this.hashDAG.merge(other.hashDAG)
        var newItemMaps = this.itemsMap

        for event <- this.hashDAG.queue ++ other.hashDAG.events.values ++ other.hashDAG.queue do
            if newHashDAG.contains(event) && !this.hashDAG.contains(event) then {
              val listItem = event.content.get
              if !newItemMaps.contains(listItem.index) then
                  newItemMaps = newItemMaps + (listItem.index -> (listItem.item, Set(event.id)))
              else {
                val currentItem = newItemMaps(listItem.index)
                var ids         = currentItem._2
                for id <- ids do
                    if newHashDAG.pathExists(id, event.id) then
                        ids = ids - id

                if !ids.exists(id => newHashDAG.pathExists(event.id, id)) then
                    ids = ids + event.id

                val chosenEventID =
                  ids.toList.sortWith((x, y) => MurmurHash3.stringHash(x) > MurmurHash3.stringHash(y)).head

                newItemMaps =
                  newItemMaps + (listItem.index -> (newHashDAG.events(chosenEventID).content.get.item, ids))
              }
            }

        LWWGrowOnlyList(newItemMaps, newHashDAG)

    /*private def list: List[ListItem[T]] =
    var events = hashDAG.events.values.filter(e => e.id == "0").toList
    var result = List.empty[Event[ListItem[T]]]
    if events.nonEmpty then
      val heads        = events.filter(e => e.content.get.pred == "0")
      val orderedHeads = heads.sortWith((x, y) => MurmurHash3.stringHash(x.id) > MurmurHash3.stringHash(y.id))
      result = orderedHeads.head :: result
      events = events.diff(heads)

    while events.nonEmpty do {
      val nextItemEvents = events.filter(e => e.content.get.pred == result.last.id).sortWith((x, y) =>
        MurmurHash3.stringHash(x.id) > MurmurHash3.stringHash(y.id)
      )
      result = nextItemEvents.head :: result
      events = events.diff(nextItemEvents)
    }

    result.map(e => e.content.get)*/

    def contains(elem: T): Boolean = itemsMap.values.exists((t, _) => elem == t)

    def empty: LWWGrowOnlyList[T] = LWWGrowOnlyList()

    def withHashDAG(hashDAG: HashDAG[ListItem[T]]): LWWGrowOnlyList[T] = this.copy(hashDAG = hashDAG)

    override def generateDelta(ids: List[String]): LWWGrowOnlyList[T] =
      LWWGrowOnlyList(Map.empty, hashDAG.getDelta(ids))

object LWWGrowOnlyList:
    def apply[T](): LWWGrowOnlyList[T] = {
      val keyPair = Ed25519Util.generateNewKeyPair
      new LWWGrowOnlyList[T](Map.empty, HashDAG[ListItem[T]](keyPair.getPublic, Some(keyPair.getPrivate)))
    }

case class ListItem[T](item: T, index: Int)
