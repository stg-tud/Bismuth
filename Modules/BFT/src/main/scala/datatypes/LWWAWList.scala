package datatypes

import crypto.Ed25519Util
import dag.{Event, HashDAG}

import scala.util.hashing.MurmurHash3

case class LWWAWList[T](
    itemsMap: Map[Int, Set[(T, String)]],
    hashDAG: HashDAG[ListOperation[T]]
) extends Replica[ListOperation[T], LWWAWList[T]]:

    lazy val list: List[T] =
      itemsMap.toList.sortWith((item1, item2) => item1._1 <= item2._1).map(x => x._2).map(set =>
        choseValueRandomlyAndDeterministically(set)
      )

    private def choseValueRandomlyAndDeterministically(set: Set[(T, String)]): T =
      set.toList.sortWith((x, y) => MurmurHash3.stringHash(x._2) > MurmurHash3.stringHash(y._2)).head._1

    def add(index: Int, element: T): LWWAWList[T] = {
      val l = list
      if index > l.size || index < 0 then
          throw Exception("illegal index")

      var hashDAGDelta = hashDAG.generateDelta(AddItem(ListItem(element, index)))
      var newHashDAG   = hashDAG.merge(hashDAGDelta)
      for i <- index until l.size do
          val delta = newHashDAG.generateDelta(AddItem(ListItem(l(i), i + 1)))
          newHashDAG = newHashDAG.merge(delta)
          hashDAGDelta = hashDAGDelta.merge(delta)

      LWWAWList(Map.empty, hashDAGDelta)
    }

    def remove(index: Int): LWWAWList[T] = {
      val l = list

      if l.isEmpty then
          throw Exception("cannot remove empty list")

      if index > l.size || index < 0 then
          throw Exception("illegal index")

      var hashDAGDelta = hashDAG.generateDelta(RemoveItem(index))
      var newHashDAG   = hashDAG.merge(hashDAGDelta)
      for i <- index + 1 until l.size do
          val delta = newHashDAG.generateDelta(AddItem(ListItem(l(i), i - 1)))
          newHashDAG = newHashDAG.merge(delta)
          hashDAGDelta = hashDAGDelta.merge(delta)

      if l.size > 1 then
          val d = newHashDAG.generateDelta(RemoveItem(l.size - 1))
          newHashDAG = newHashDAG.merge(d)
          hashDAGDelta = hashDAGDelta.merge(d)

      LWWAWList(Map.empty, hashDAGDelta)
    }

    override def merge(other: LWWAWList[T]): LWWAWList[T] =
        val newHashDAG  = this.hashDAG.merge(other.hashDAG)
        var newItemMaps = this.itemsMap
        var orderedEvents = List.empty[Event[ListOperation[T]]]

        for event <- this.hashDAG.queue ++ other.hashDAG.events.values ++ other.hashDAG.queue do
          if newHashDAG.contains(event) && !this.hashDAG.contains(event) then
            orderedEvents = event :: orderedEvents

        orderedEvents = newHashDAG.orderEvents(orderedEvents)

        for event <- orderedEvents do
            if newHashDAG.contains(event) && !this.hashDAG.contains(event) then {
              val op = event.content.get
              op match
                  case AddItem(listItem) =>
                    if !newItemMaps.contains(listItem.index) then
                        newItemMaps = newItemMaps + (listItem.index -> Set((listItem.item, event.id)))
                    else {
                      var currentItems = newItemMaps(listItem.index)
                      var ids          = currentItems.map((t, id) => id)
                      for id <- ids do
                          if newHashDAG.pathExists(id, event.id) then {
                            ids = ids - id
                            currentItems = currentItems.filterNot { case (_, s) => s == id }
                          }

                      if !ids.exists(id => newHashDAG.pathExists(event.id, id)) then {
                        ids = ids + event.id
                        currentItems = currentItems + ((listItem.item, event.id))
                      }

                      newItemMaps = newItemMaps + (listItem.index -> currentItems)
                    }

                  case RemoveItem(index) =>
                    if newItemMaps.contains(index) then
                        var currentItems = newItemMaps(index)
                        var ids          = currentItems.map((t, id) => id)
                        for id <- ids do
                            if newHashDAG.pathExists(id, event.id) then
                                ids = ids - id
                                currentItems = currentItems.filterNot { case (_, s) => s == id }

                        if ids.isEmpty then
                            newItemMaps = newItemMaps - index
                        else
                            newItemMaps = newItemMaps + (index -> currentItems)
            }

        LWWAWList(newItemMaps, newHashDAG)

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

    def contains(elem: T): Boolean = list.contains(elem)

    def empty: LWWAWList[T] = LWWAWList()

    def withHashDAG(hashDAG: HashDAG[ListOperation[T]]): LWWAWList[T] = this.copy(hashDAG = hashDAG)

    override def generateDelta(ids: List[String]): LWWAWList[T] =
      LWWAWList(Map.empty, hashDAG.getDelta(ids))

object LWWAWList:
    def apply[T](): LWWAWList[T] = {
      val keyPair = Ed25519Util.generateNewKeyPair
      new LWWAWList[T](Map.empty, HashDAG[ListOperation[T]](keyPair.getPublic, Some(keyPair.getPrivate)))
    }

sealed trait ListOperation[T]
case class RemoveItem[T](index: Int)         extends ListOperation[T]
case class AddItem[T](listItem: ListItem[T]) extends ListOperation[T]
