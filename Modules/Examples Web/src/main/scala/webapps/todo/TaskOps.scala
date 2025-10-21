package webapps.todo

import rdts.base.LocalUid
import rdts.datatypes.ReplicatedList
import rdts.syntax.DeltaBuffer
import reactives.default.*
import reactives.operator.Fold.Branch
import webapps.todo.TodoDataManager.TodoRepState

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.unused

object TaskOps {
  def resetBuffer[T]: Branch[DeltaBuffer[T]] = Fold.Branch[DeltaBuffer[T]](Nil, isStatic = false, _ => Fold.current.clearDeltas())
}

// `taskrefs` is unused as a reference, but is used indirectly so this parameter serves as a requirement
// that a `taskrefs` needs to be created before taskops may be used
class TaskOps(@unused taskrefs: TaskReferences, replicaID: LocalUid) {

  type State = DeltaBuffer[ReplicatedList[TaskRef]]

  given LocalUid = replicaID

  def handleCreateTodo(createTodo: Event[String]): Fold.Branch[State] = createTodo.branch { desc =>
    val taskid = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"
    TaskReferences.lookupOrCreateTaskRef(taskid, Some(TaskData(desc)))
    val taskref = TaskRef(taskid)
    current.mod(_.prepend(taskref))
  }

  def handleRemoveAll(removeAll: Event[Any]): Fold.Branch[State] =
    removeAll.branch: _ =>
      current.mod(_.deleteBy { (taskref: TaskRef) =>
        val isDone = taskref.task.value.state.read.exists(_.done)
        // todo, move to observer, disconnect during transaction does not respect rollbacks
        if isDone then taskref.task.disconnect()
        isDone
      })

  def handleRemove(state: State)(id: String): State = {
    state.mod(_.deleteBy { (taskref: TaskRef) =>
      val delete = taskref.id == id
      // todo, move to observer, disconnect during transaction does not respect rollbacks
      if delete then taskref.task.disconnect()
      delete
    })
  }

  def handleDelta(deltaEvent: Event[TodoDataManager.TodoRepState]): Fold.Branch[State] =
    deltaEvent.branch { allDeltas =>
      val deltaBuffered = current

      val delta = (allDeltas: TodoRepState).list

      val newList = deltaBuffered.applyDelta(delta)

      val oldIDs = deltaBuffered.state.toList.toSet
      val newIDs = newList.state.toList.toSet

      val removed = oldIDs -- newIDs
      removed.foreach { _.task.disconnect() }

      newList
    }

}
