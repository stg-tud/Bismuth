package test.rdts.bespoke

import rdts.datatypes.RemoveWinsArray as ReplicatedList
import rdts.datatypes.LastWriterWins as LWW
import rdts.base.Lattice
import rdts.experiments.UndoRedoReplica
import rdts.datatypes.ReplicatedTree
import rdts.time.Dot
import rdts.base.LocalUid
import rdts.base.Bottom
import rdts.experiments.DeltaHistory

object TaskList {
  def taskList(tree: ReplicatedTree[Entry], dot: Dot): Option[TaskList] =
    tree.node(dot).map(_.value).flatMap {
      case Entry.TaskListEntry(list) => Some(list)
      case _                         => None
    }

  def updateTaskList(tree: ReplicatedTree[Entry], id: Dot, f: TaskList => TaskList)(using LocalUid) =
    tree.update(
      id,
      Entry.TaskListEntry(f(tree.node(id).get.value.asInstanceOf[Entry.TaskListEntry].list))
    )

  def updateFolder(tree: ReplicatedTree[Entry], id: Dot, f: Folder => Folder)(using LocalUid) =
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

    def updateFolderName(folder: Dot, newName: String)(using LocalUid) =
      state.mod(tree => updateFolder(tree, folder, f => f.copy(name = LWW.now(newName))))

    def updateTaskListName(id: Dot, newName: String)(using LocalUid) =
      state.mod(tree => updateTaskList(tree, id, tl => tl.copy(name = LWW.now(newName), items = ReplicatedList.empty)))

    def addTaskList(id: Dot, item: Task)(using LocalUid) =
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

class TaskListTest extends munit.FunSuite {
  import TaskList.*

  test("example") {
    given aid: LocalUid = LocalUid.predefined("a")
    given bid: LocalUid = LocalUid.predefined("b")

    // Initialize two replicas
    var replica1 = App(UndoRedoReplica.empty[ReplicatedTree[Entry]])
    var replica2 = App(UndoRedoReplica.empty[ReplicatedTree[Entry]])

    val root = ReplicatedTree.rootDot

    // Replica 1: Create initial folder structure
    val folder1Delta = replica1.addFolder(root, "Work")(using aid)
    val folder1Id    = replica1.read.children(root).head.dot

    // Replica 2: Create another folder concurrently
    val folder2Delta = replica2.addFolder(root, "Personal")(using bid)
    val folder2Id    = replica2.read.children(root).head.dot

    // Sync replicas
    replica1.applyDelta(folder2Delta)
    replica2.applyDelta(folder1Delta)

    // Verify both replicas have both folders
    assertEquals(replica1.read.children(root).size, 2)
    assertEquals(replica2.read.children(root).size, 2)

    // Replica 1: Create a task list in folder1
    val taskList1Delta = replica1.addTaskList(folder1Id, "Sprint Tasks")(using aid)
    val taskList1Id    = replica1.read.children(folder1Id).head.dot

    // Sync to replica 2
    replica2.applyDelta(taskList1Delta)

    // Replica 1: Add tasks to the list
    val task1 = Task(LWW.now("Implement feature X"), LWW.now(Some("Add new functionality")))
    val task2 = Task(LWW.now("Fix bug Y"), LWW.now(Some("Critical bug in production")))

    val addTask1Delta = replica1.addTaskList(taskList1Id, task1)(using aid)
    val addTask2Delta = replica1.addTaskList(taskList1Id, task2)(using aid)

    // Replica 2: Concurrently add task to the same list
    val task3         = Task(LWW.now("Write documentation"), LWW.now(Some("Update user guide")))
    val addTask3Delta = replica2.addTaskList(taskList1Id, task3)(using bid)

    // Sync replicas
    replica1.applyDelta(addTask3Delta)
    replica2.applyDelta(addTask1Delta)
    replica2.applyDelta(addTask2Delta)

    // Both replicas should have 3 tasks
    assertEquals(taskList(replica1.read, taskList1Id).get.items.toList.size, 3)
    assertEquals(taskList(replica2.read, taskList1Id).get.items.toList.size, 3)

    // Replica 1: Update task title
    val updateTitleDelta = replica1.updateTaskTitle(taskList1Id, 0, "Implement feature X - Updated")(using aid)

    // Replica 2: Update task description concurrently
    val updateDescDelta = replica2.updateTaskDescription(taskList1Id, 1, Some("High priority bug fix"))(using bid)

    // Sync replicas
    replica1.applyDelta(updateDescDelta)
    replica2.applyDelta(updateTitleDelta)

    // Replica 1: Rename task list
    val renameListDelta = replica1.updateTaskListName(taskList1Id, "Sprint 1 Tasks")(using aid)

    // Sync to replica 2
    replica2.applyDelta(renameListDelta)

    // Verify final state is consistent
    val finalList1 = taskList(replica1.read, taskList1Id).get
    val finalList2 = taskList(replica2.read, taskList1Id).get

    assertEquals(finalList1.name.read, "Sprint 1 Tasks")
    assertEquals(finalList2.name.read, "Sprint 1 Tasks")

    // Replica 1: Move task within list
    val moveTaskDelta = replica1.moveTaskListItem(taskList1Id, 2, 0)(using aid)

    // Sync to replica 2
    replica2.applyDelta(moveTaskDelta)

    // Replica 1: Remove a task
    val removeTaskDelta = replica1.removeTaskListItem(taskList1Id, 1)(using aid)

    // Sync to replica 2
    replica2.applyDelta(removeTaskDelta)

    // Verify both replicas have 2 tasks now
    assertEquals(taskList(replica1.read, taskList1Id).get.items.toList.size, 2)
    assertEquals(taskList(replica2.read, taskList1Id).get.items.toList.size, 2)

    // Move task list to another folder
    val moveEntryDelta = replica1.moveEntry(taskList1Id, folder2Id)(using aid)
    replica2.applyDelta(moveEntryDelta)

    // Verify task list moved
    assertEquals(replica1.read.children(folder1Id).size, 0)
    assertEquals(replica1.read.children(folder2Id).size, 1)

    assertEquals(replica1.read, replica2.read)
  }
}
