package ex2021encfxtodo

import javafx.collections.{FXCollections, ObservableList}
import rdts.base.LocalUid
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object TodoListController {
  val replicaId: LocalUid = LocalUid.gen()

  private val crdt: SyncedTodoListCrdt = new SyncedTodoListCrdt(replicaId, handleUpdated)

  def handleUpdated(before: Map[UUID, TodoEntry], after: Map[UUID, TodoEntry]): Unit = {
    Platform.runLater {
      val added   = after.keySet.diff(before.keySet)
      val removed = before.keySet.diff(after.keySet)
      val changed = (before.keySet intersect after.keySet)
        .iterator
        .flatMap { uuid =>
          val next = after(uuid)
          Option.when(before(uuid) != next)(uuid -> next)
        }
        .toMap

      added.foreach { uuid =>
        uuidToTodoEntryProperties.getOrElseUpdate(uuid, ObjectProperty(after(uuid)))
        if !observableUuidList.contains(uuid) then observableUuidList.add(uuid): Unit
      }

      changed.foreach { case (uuid, entry) =>
        uuidToTodoEntryProperties.get(uuid) match
            case Some(property) => property.set(entry)
            case None           =>
              uuidToTodoEntryProperties.put(uuid, ObjectProperty(entry))
              if !observableUuidList.contains(uuid) then observableUuidList.add(uuid): Unit
      }

      removed.foreach { uuid =>
        observableUuidList.remove(uuid)
        uuidToTodoEntryProperties.remove(uuid)
      }
    }
  }

  private def applyLocalChange(update: => Unit): Unit = {
    val before = crdt.values
    update
    val after = crdt.values
    handleUpdated(before, after)
  }

  val observableUuidList: ObservableList[UUID] =
    FXCollections.observableList(new java.util.ArrayList[UUID](crdt.values.keys.asJavaCollection))

  private val uuidToTodoEntryProperties: mutable.Map[UUID, ObjectProperty[TodoEntry]] =
    new ConcurrentHashMap[UUID, ObjectProperty[TodoEntry]]().asScala

  def getTodo(uuid: UUID): Option[ObjectProperty[TodoEntry]] =
    uuidToTodoEntryProperties.get(uuid)

  def addTodo(todoEntry: TodoEntry): Unit = {
    val uuid = UUID.randomUUID()
    applyLocalChange {
      crdt.put(uuid, todoEntry)
    }
    ()
  }

  def removeTodo(uuid: UUID): Unit = {
    applyLocalChange {
      crdt.remove(uuid)
    }
    ()
  }

  def changeTodo(uuid: UUID, changedEntry: TodoEntry): Unit = {
    if uuid == null || changedEntry == null then {
      println("uuid null for " + changedEntry)
    } else {
      val oldTodo = crdt.get(uuid)
      if oldTodo.isEmpty || oldTodo.contains(changedEntry) then return

      applyLocalChange {
        crdt.put(uuid, changedEntry)
      }
    }
  }

  def stop(): Unit =
    crdt.shutdown()

  def connectionString: String = crdt.address

  def connect(connectionString: String): Unit = crdt.connect(connectionString)

  def todos: Map[UUID, TodoEntry] = crdt.values

  def remoteAddresses: Set[String] = crdt.remoteAddresses
}
