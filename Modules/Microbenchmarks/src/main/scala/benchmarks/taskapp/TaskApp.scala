package benchmarks.taskapp

import rdts.datatypes.LastWriterWins as LWW
import rdts.base.{Lattice, LocalUid, Bottom}
import rdts.experiments.UndoRedoReplica
import rdts.experiments.DeltaHistory
import rdts.datatypes.ReplicatedTree
import rdts.time.Dot
import rdts.experiments.RemoveWinsArrayExperiment as ReplicatedList

object TaskApp {
  def taskList(tree: ReplicatedTree[Entry], dot: Dot): Option[TaskList] =
    tree.node(dot).map(_.value).flatMap {
      case Entry.TaskListEntry(list) => Some(list)
      case _                         => None
    }

  def updateTaskList(tree: ReplicatedTree[Entry], id: Dot, f: TaskList => TaskList) =
    tree.update(
      id,
      Entry.TaskListEntry(f(tree.node(id).get.value.asInstanceOf[Entry.TaskListEntry].list))
    )

  def updateFolder(tree: ReplicatedTree[Entry], id: Dot, f: Folder => Folder) =
    tree.update(
      id,
      Entry.FolderEntry(
        f(tree.node(id).get.value.asInstanceOf[Entry.FolderEntry].folder)
      )
    )

  case class App(state: UndoRedoReplica[ReplicatedTree[Entry]]) {
    def read: ReplicatedTree[Entry] = state.state

    def applyDelta(delta: DeltaHistory[ReplicatedTree[Entry]]): App =
        this.state.receive(delta)
        this

    def addTaskList(parentFolder: Dot, name: String)(using LocalUid) =
      state.mod(tree =>
        tree.insertWith(
          parentFolder,
          id =>
            Entry.TaskListEntry(
              TaskList(
                name = LWW.now(name),
                items = ReplicatedList.empty
              )
            )
        )
      )

    def addFolder(parentFolder: Dot, name: String)(using LocalUid) =
      state.mod(tree =>
        tree.insertWith(
          parentFolder,
          id =>
            Entry.FolderEntry(
              Folder(
                name = LWW.now(name),
              )
            )
        )
      )

    def moveEntry(entryId: Dot, newParent: Dot)(using LocalUid) =
      state.mod(tree => tree.move(entryId, newParent))

    def removeEntry(entryId: Dot)(using LocalUid) =
      state.mod(tree => tree.delete(entryId))

    def updateFolderName(folder: Dot, newName: String)(using LocalUid) =
      state.mod(tree => updateFolder(tree, folder, f => f.copy(name = LWW.now(newName))))

    def updateTaskListName(id: Dot, newName: String)(using LocalUid) =
      state.mod(tree =>
        updateTaskList(tree, id, tl => tl.copy(name = LWW.now(newName), items = ReplicatedList.empty))
      )

    def addTaskListItem(id: Dot, item: Task)(using LocalUid) =
      state.mod(tree => updateTaskList(tree, id, tl => tl.copy(items = tl.items.append(item))))

    def removeTaskListItem(id: Dot, itemIx: Int)(using LocalUid) =
      state.mod(tree => updateTaskList(tree, id, tl => tl.copy(items = tl.items.remove(itemIx))))

    def moveTaskListItem(id: Dot, from: Int, to: Int)(using LocalUid) =
      state.mod(tree => updateTaskList(tree, id, tl => tl.copy(items = tl.items.move(from, to))))

    def updateTaskTitle(taskListId: Dot, itemIx: Int, newTitle: String)(using LocalUid) =
      state.mod(tree =>
        updateTaskList(
          tree,
          taskListId,
          tl => tl.copy(items = tl.items.updateWith(itemIx, item => item.copy(title = LWW.now(newTitle))))
        )
      )

    def updateTaskDescription(taskListId: Dot, itemIx: Int, newDescription: Option[String])(using LocalUid) =
      state.mod(tree =>
        updateTaskList(
          tree,
          taskListId,
          tl => tl.copy(items = tl.items.updateWith(itemIx, item => item.copy(description = LWW.now(newDescription))))
        )
      )

    def updateTaskDone(taskListId: Dot, itemIx: Int, done: Boolean)(using LocalUid) =
      state.mod(tree =>
        updateTaskList(
          tree,
          taskListId,
          tl => tl.copy(items = tl.items.updateWith(itemIx, item => item.copy(done = LWW.now(done))))
        )
      )

    def forEachTaskListItem(taskListId: Dot, f: Task => Task)(using LocalUid) =
      state.mod(tree =>
        updateTaskList(
          tree,
          taskListId,
          tl => tl.copy(items = tl.items.apply(f))
        )
      )
  }

  enum Entry:
      case FolderEntry(folder: Folder)
      case TaskListEntry(list: TaskList)

  case class Folder(val name: LWW[String])
  case class TaskList(
      val name: LWW[String],
      val items: ReplicatedList[Task]
  )

  case class Task(
      val title: LWW[String],
      val description: LWW[Option[String]],
      val done: LWW[Boolean] = LWW.now(false)
  )

  given stateBottom: Bottom[ReplicatedTree[Entry]] = Bottom.provide(ReplicatedTree.empty[Entry])
  given entryLattice: Lattice[Entry] with {
    def merge(left: Entry, right: Entry): Entry = (left, right) match {
      case (Entry.FolderEntry(lf), Entry.FolderEntry(rf)) =>
        Entry.FolderEntry(Lattice.merge(lf, rf))
      case (Entry.TaskListEntry(ll), Entry.TaskListEntry(rl)) =>
        Entry.TaskListEntry(Lattice.merge(ll, rl))
      case _ =>
        throw new IllegalArgumentException("Cannot merge different Entry types")
    }
  }
  given folderLattice: Lattice[Folder]     = Lattice.derived
  given taskListLattice: Lattice[TaskList] = Lattice.derived
  given taskLattice: Lattice[Task]         = Lattice.derived
}
